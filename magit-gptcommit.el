;;; magit-gptcommit.el --- Git commit with help of gpt -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Tiou Lims

;; Author: Tiou Lims <dourokinga@gmail.com>
;; URL: https://github.com/douo/magit-gptcommit
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (dash "2.13.0") (magit "2.90.1") (llm "0.16.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides a way to commit with help of gpt.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'dash)
(require 'eieio)
(require 'magit)
(require 'llm)

;; Debugging facilities
(defcustom magit-gptcommit-debug nil
  "When non-nil, enable detailed debugging for magit-gptcommit.
This will log detailed information about worker operations, callbacks,
and state changes to help diagnose race conditions when staging
changes quickly."
  :type 'boolean
  :group 'magit-gptcommit)

(defvar magit-gptcommit--debug-buffer-name "*magit-gptcommit-debug*"
  "Name of the buffer where debug messages are logged.")

(defun magit-gptcommit--debug (format-string &rest args)
  "Log a debug message if debugging is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (when magit-gptcommit-debug
    (let ((buf (get-buffer-create magit-gptcommit--debug-buffer-name))
          (msg (apply #'format format-string args)))
      (with-current-buffer buf
        (goto-char (point-max))
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              (buffer-undo-list t)
              (timestamp (format-time-string "%H:%M:%S.%3N")))
          (insert (format "[%s] %s\n" timestamp msg)))))))

(defun magit-gptcommit--safe-to-delete-p (start-pos end-pos &optional buffer)
  "Check if it's safe to delete region between START-POS and END-POS in BUFFER.
If BUFFER is nil, use the current buffer."
  (let* ((buf (or buffer (current-buffer)))
         (buf-size (with-current-buffer buf (buffer-size)))
         ;; Use token size as a reasonable limit for commit message size
         ;; 4 chars per token is the approximation from the code
         (max-chars-per-token 4)
         (max-tokens (or magit-gptcommit-max-token 4096))
         (max-commit-size (* max-chars-per-token max-tokens))
         (region-size (- end-pos start-pos))
         ;; Ensure we never delete more than 80% of the buffer
         (max-safe-deletion (min max-commit-size
                                (/ (* buf-size 80) 100))))

    ;; Basic sanity checks for marker positions
    (and (bufferp buf)
         (> start-pos 1) ;; Position 1 is suspicious
         (>= end-pos start-pos)
         (<= region-size max-safe-deletion)
         ;; We want to delete SOME content, but not too much
         (> region-size 0))))

(defun magit-gptcommit--safe-delete-region (start end &optional buffer)
  "Safely delete region between START and END if it passes safety checks.
If BUFFER is nil, operate on current buffer.
Returns t if deletion was performed, nil otherwise."
  (if (magit-gptcommit--safe-to-delete-p start end buffer)
      (progn
        (if buffer
            (with-current-buffer buffer
              (delete-region start end))
          (delete-region start end))
        t)
    ;; Deletion not performed due to safety concerns
    (magit-gptcommit--debug "⚠️ PREVENTED UNSAFE DELETION: start=%s end=%s size=%s, buffer-size=%s"
                           start end (- end start) (buffer-size))
    nil))

;;;###autoload
(define-minor-mode magit-gptcommit-mode
  "Magit gptcommit mode."
  :require 'magit-gptcommit
  :group 'magit-gptcommit
  :global t
  (if magit-gptcommit-mode
      (progn
        (magit-add-section-hook 'magit-status-sections-hook
                                #'magit-gptcommit--status-insert-gptcommit
                                nil
                                'append))
    ;; Disable mode
    (remove-hook 'magit-status-sections-hook #'magit-gptcommit--status-insert-gptcommit)))

(defvar magit-gptcommit--last-message nil
  "GPT generated commit message for current repository.")
(defvar magit-gptcommit--active-section-list nil
  "List of active gptcommit sections for current repository.")
(defvar magit-gptcommit--active-worker nil
  "Running gptcommit process for current repository.
Stored as a cons cell (PROCESS . RESPONSE) where RESPONE is a SSO Message.")

(cl-defstruct magit-gptcommit--worker
  "Structure respesenting current active llm request."
  key llm-request message sections
  (active t :read-only nil)
  (creation-time (float-time) :read-only t))

(defconst magit-gptcommit-prompt-one-line "You are an expert programmer writing a Git commit message.
You have carefully reviewed every file diff included in this commit.

First, choose the most appropriate label for the changes.
Here are the labels you can choose from:
- build: Changes that affect the build system or external dependencies (e.g., gulp, broccoli, npm)
- chore: Routine tasks like updating dependencies, licenses, or repo settings
- ci: Changes to CI configuration files or scripts (e.g., GitHub Actions, CircleCI)
- docs: Documentation-only changes (e.g., fixing typos, adding examples)
- feat: Introduces a new feature to the codebase
- fix: Patches a bug in the codebase
- perf: Improves performance without changing behavior
- refactor: Code changes that neither fix bugs nor add features
- style: Non-functional changes like formatting or whitespace
- test: Adds or corrects tests

Next, write a high-level summary of the commit.
- Keep it to a single line, no more than 50 characters
- Use the imperative tense (e.g., 'Add logging' not 'Added logging')
- Ensure the message reflects a clear and cohesive change
- Do not end the summary with a period
- Do not use backticks (`) anywhere in the response

THE FILE DIFFS:
```
%s
```
Now, write the commit message using this format: [label]: [summary]")

(defcustom magit-gptcommit-prompt magit-gptcommit-prompt-one-line
  "The prompt that was used to generate the commit message."
  :type 'string
  :group 'magit-gptcommit)

;; (defun magit-gptcommit-load-prompt ()
;;   "Load prompt from file or download from url."
;;   (let* ((directory (expand-file-name "assets/magit-gptcommit/" user-emacs-directory))
;;          (file-path (expand-file-name "prompt.txt" directory))
;;          (url "https://example.com/prompt.txt"))
;;     (unless (file-exists-p directory)
;;       (make-directory directory t))
;;     (if (file-exists-p file-path)
;;         (with-temp-buffer
;;           (insert-file-contents file-path)
;;           (setq magit-gptcommit-prompt (buffer-string)))
;;       ;; TODO download from url
;;       (url-retrieve url (lambda (status)
;;                           (when (equal (car status) :ok)
;;                             (with-temp-buffer (url-retrieve-sentinel (current-buffer))
;;                                               (write-region (point-min) (point-max) file-path)
;;                                               (setq magit-gptcommit-prompt (buffer-string)))))))))

(defcustom magit-gptcommit-max-token 4096
  "Max token length."
  :type 'integer
  :group 'magit-gptcommit)

(defcustom magit-gptcommit-determine-max-token nil
  "Whether to determine the max token for the llm provider.

Max tokens set by the llm provider are used only if `magit-gptcommit-max-token'
is nil."
  :type 'boolean
  :group 'magit-gptcommit)

(defcustom magit-gptcommit-llm-provider nil
  "llm provider to use"
  :type '(choice
          (sexp :tag "llm provider")
          (function :tag "Function that returns an llm provider."))
  :group 'magit-gptcommit)

(defcustom magit-gptcommit-llm-provider-temperature nil
  "llm provider temperature."
  :type 'float
  :group 'magit-gptcommit)

(defcustom magit-gptcommit-llm-provider-max-tokens nil
  "llm provider max tokens to generate."
  :type 'integer
  :group 'magit-gptcommit)

(defcustom magit-gptcommit-process-commit-message-function #'magit-gptcommit--process-commit-message
  "Commit message function."
  :type 'function
  :group 'magit-gptcommit)

(defcustom magit-gptcommit-commit-message-action 'replace
  "Commit message action."
  :type '(choice
          (const append)
          (const prepend)
          (const replace))
  :group 'magit-gptcommit)

;;; Cache
(defvar magit-gptcommit-cache-limit 4096
  "Max number of cache entries.")

(defcustom magit-gptcommit-message-timeout 16
  "Number of seconds before messages expire from the message history.
Set to nil to disable message expiration."
  :type '(choice (integer :tag "Seconds before expiration")
                 (const :tag "Never expire" nil))
  :group 'magit-gptcommit)

(defcustom magit-gptcommit-request-cooldown 0.01
  "Time in seconds to wait after an LLM request completes before allowing another.
This helps prevent `FTP access denied' errors by ensuring we don't
make too many requests in quick succession."
  :type 'number
  :group 'magit-gptcommit)

(defvar magit-gptcommit--request-in-progress nil
  "Non-nil when an LLM request is currently in progress.
Used to serialize requests and prevent concurrent API calls.")

(defvar magit-gptcommit--last-request-end-time 0
  "Timestamp of when the last LLM request completed.")

(defvar magit-gptcommit--last-messages (make-hash-table :test 'equal :size 20)
  "Hash table to store recent messages from workers by key.
Value is (CREATION-TIME . MESSAGE) where CREATION-TIME is used to find the newest.")

(defvar magit-gptcommit--cache (make-hash-table :test 'equal :size magit-gptcommit-cache-limit)
  "Cache of generated commit messages as a hash table.")

(cl-defun magit-gptcommit--cache-key (content &optional (repository (magit-repository-local-repository)))
  "Return cache key for CONTENT and REPOSITORY."
  ;; set repository to default if not initial
  (md5 (format "%s%s" repository content)))

(defun magit-gptcommit--cache-set (key value)
  "Set cache VALUE for KEY."
  ;; If cache is full, remove half of the entries
  (when (>= (hash-table-count magit-gptcommit--cache) magit-gptcommit-cache-limit)
    (let* ((keys (hash-table-keys magit-gptcommit--cache))
           (half-count (/ (length keys) 2)))
      (dolist (k (seq-take keys half-count))
        (remhash k magit-gptcommit--cache))))
  ;; Add the new value
  (puthash key value magit-gptcommit--cache))

(defun magit-gptcommit--cache-get (key &optional default)
  "Return cache value for KEY or DEFAULT if not found."
  (gethash key magit-gptcommit--cache default))

(defun magit-gptcommit--cache-p (key)
  "Non-nil when a value exists for KEY.

Return a (KEY . VALUE) cons cell for compatibility with the old implementation."
  (let ((value (gethash key magit-gptcommit--cache 'not-found)))
    (unless (eq value 'not-found)
      (cons key value))))

(defun magit-gptcommit--find-newest-message ()
  "Find the most recent message from all workers.
Returns the message text or nil if no messages are available.
Also removes expired messages from the hash table."
  (let ((newest-time 0)
        (newest-msg nil)
        (current-time (float-time))
        (expired-keys '()))

    ;; Find the newest message and track expired messages
    (maphash
     (lambda (key value)
       (let ((time (car value))
             (msg (cdr value)))
         ;; Check for expired messages if timeout is set
         (when (and magit-gptcommit-message-timeout
                    (> (- current-time time) magit-gptcommit-message-timeout))
           (push key expired-keys)
           (magit-gptcommit--debug "Message with key %s expired (age: %.1f seconds)"
                                  key (- current-time time)))

         ;; Track the newest message
         (when (> time newest-time)
           (setq newest-time time)
           (setq newest-msg msg))))
     magit-gptcommit--last-messages)

    ;; Remove expired messages
    (dolist (key expired-keys)
      (remhash key magit-gptcommit--last-messages)
      (magit-gptcommit--debug "Removed expired message with key %s" key))

    newest-msg))


;;; utils
(defun magit-gptcommit-status-buffer-setup ()
  "Setup gptcommit transient command in `magit-status-mode' buffer."
  (interactive)
  (transient-append-suffix 'magit-commit '(1 -1)
    ["GPT Commit"
     :if magit-anything-staged-p
     ("G" "Generate" magit-gptcommit-generate)
     ("Q" "Quick Accept" magit-gptcommit-commit-quick)
     ("C" "Accept" magit-gptcommit-commit-create)
     ]))

;; credited: https://emacs.stackexchange.com/a/3339/30746
(defmacro magit-gptcommit--add-hook-run-once (hook function &optional append local)
  "Like `add-hook', but remove the HOOK after FUNCTION is called.
APPEND and LOCAL have the same meaning as in `add-hook'."
  (let ((sym (make-symbol "#once")))
    `(progn
       (defun ,sym ()
         (remove-hook ,hook ',sym ,local)
         (funcall ,function))
       (add-hook ,hook ',sym ,append ,local))))

(defmacro magit-gptcommit--update-heading-status (status face)
  "Update gptcommit section heading with STATUS and FACE."
  `(progn
     (goto-char (+ 12 start))
     (delete-region (point) (pos-eol))
     (insert (propertize ,status 'font-lock-face ,face))))

(cl-defun magit-gptcommit--move-last-to-position (list position)
  "Move the last element of LIST to POSITION."
  (append (take position list) (last list) (butlast (-drop position list))))

(cl-defun magit-gptcommit--goto-target-position (&optional (condition '(tags tag branch))) ;;
  "Return end position of section after which to insert the commit message.
Position is determined by CONDITION, which is defined in `magit-section-match'."
  (let ((children (oref magit-root-section children))
        (pos 0))
    ;; iterate over all children
    (cl-loop for child in children
             ;; find the first child that matches the condition
             if (or (null condition)
                    (magit-section-match condition child))
             return (goto-char (identity (oref child start)))
             do (cl-incf pos))  ;; index after target section
    (when (< pos (length children))
      pos)))

(cl-defun magit-gptcommit--goto-target-section (&optional (condition '(tags tag branch))) ;;
  "Return end position of section after which to insert the commit message.
SECTION is determined by CONDITION, which is defined in `magit-section-match'."
  (let ((children (oref magit-root-section children))
        (target))
    ;; iterate over all children
    (setq target (cl-loop for child in children
                          ;; find the first child that matches the condition
                          if (or (null condition)
                                 (magit-section-match condition child))
                          return child))
    (when target
      (goto-char (oref target start))
      target)))


(defun magit-gptcommit--retrieve-staged-diff ()
  "Retrieve staged diff assuming current magit section is the staged section."
  (let* ((section (magit-current-section))
         (max-token (if (and magit-gptcommit-determine-max-token (not magit-gptcommit-max-token))
                        (llm-chat-token-limit (magit-gptcommit--llm-provider))
                      magit-gptcommit-max-token))
         ;; HACK:  1 token ~= 4 chars
         (max-char (- (* 4 max-token) (length magit-gptcommit-prompt)))
         (diffs (mapcar
                 (lambda (child)
                   (with-slots (start end) child
                     (cons start
                           (- (marker-position end) (marker-position start)))))
                 (oref section children)))
         (total (with-slots (content end) section
                  (- (marker-position end) (marker-position content)))))
    (if (> total max-char)
        (mapconcat
         (lambda (child)
           (buffer-substring-no-properties (car child)
                                           (+ (marker-position (car child))
                                              (floor (* max-char (/ (float (cdr child)) total))))))
         diffs "\n")
      (with-slots (content end) section
        (buffer-substring-no-properties content end)))))

(cl-defun magit-gptcommit--running-p (&optional (repository (magit-repository-local-repository)))
  "Return non-nil if gptcommit is running for current REPOSITORY."
  (magit-repository-local-exists-p 'magit-gptcommit--active-worker repository))


(defun magit-gptcommit--status-insert-gptcommit ()
  "Insert gptcommit section into status buffer."
  (magit-gptcommit--insert 'staged))

(defun magit-gptcommit--insert (condition &optional no-cache)
  "Insert gptcommit section above staged section.
Staged section position is determined by CONDITION,
which is defined in `magit-section-match'.
NO-CACHE is non-nil if cache should be ignored."
  (magit-gptcommit--debug "Inserting gptcommit section with condition: %s, no-cache: %s"
                          condition no-cache)
  (if (not magit-gptcommit-llm-provider)
      (progn
        (magit-gptcommit--debug "No LLM provider configured")
        (error "No llm provider, please configure `magit-gptcommit-llm-provider'."))
    (save-excursion
      (when-let ((buf (current-buffer))
                 ;; generated if staged section exists
                 ;; TODO: magit-anything-staged-p
                 (pos (magit-gptcommit--goto-target-position condition))
                 (inhibit-read-only t)
                 (magit-insert-section--parent magit-root-section))

        (magit-repository-local-delete 'magit-gptcommit--last-message)
        (let* ((diff (magit-gptcommit--retrieve-staged-diff))
               (key (magit-gptcommit--cache-key diff))
               (worker (magit-repository-local-get 'magit-gptcommit--active-worker))
               (oldkey (and worker (magit-gptcommit--worker-key worker))))
          (if-let ((msg (and (not no-cache) (magit-gptcommit--cache-get key))))
              ;; cache hit
              (progn
                ;; If a worker is running, abort it first when using cached message
                (when (magit-gptcommit--running-p)
                  (magit-gptcommit--debug "Aborting any existing workers before using cached message")
                  (magit-gptcommit-abort))
                ;; Save message to last-messages table for consistency with streaming API
                (puthash key (cons (float-time) msg) magit-gptcommit--last-messages)
                ;; Insert the section with cached content
                (magit-insert-section (gptcommit nil nil)
                  (magit-insert-heading
                    (format
                     (propertize "GPT commit: %s" 'font-lock-face 'magit-section-heading)
                     (propertize "Cache" 'font-lock-face 'success)))
                  ;; Use the helper function for consistent formatting
                  (magit-gptcommit--insert-message (point) msg)
                  (magit-repository-local-set 'magit-gptcommit--last-message msg)))
            ;; cache miss
            (magit-insert-section (gptcommit nil nil)
              (magit-insert-heading
                (format
                 (propertize "GPT commit: %s" 'font-lock-face 'magit-section-heading)
                 (propertize "Waiting" 'font-lock-face 'warning)))
              (if (and worker (equal key oldkey))
                  ;; if yes, then just insert the generated commit message
                  (progn
                    ;; First, remove any existing section to avoid duplicates
                    (magit-gptcommit-remove-section)
                    (setf (magit-gptcommit--worker-sections worker)
                          (assq-delete-all buf (magit-gptcommit--worker-sections worker)))
                    ;; Use the helper function for consistent formatting
                    (magit-gptcommit--insert-message (point)
                                               (magit-gptcommit--worker-message
                                                (magit-repository-local-get 'magit-gptcommit--active-worker))))
                (when worker
                  (magit-gptcommit-abort))
                (let ((start-position (point-marker))
                      (tracking-marker (point-marker)))
                  (set-marker-insertion-type start-position nil)
                  (set-marker-insertion-type tracking-marker t)
                  (insert "\n")
                  (magit-gptcommit--llm-chat-streaming
                   key
                   (list :prompt (format magit-gptcommit-prompt diff)
                         :buffer buf
                         :position start-position
                         :tracking-marker tracking-marker)
                   #'magit-gptcommit--stream-insert-response))))
            ;; store section in repository-local active worker
            (let ((section (car (last (oref magit-root-section children))))
                  (worker (magit-repository-local-get 'magit-gptcommit--active-worker)))
              (setf (magit-gptcommit--worker-sections worker)
                    (cons (cons buf section)
                          (magit-gptcommit--worker-sections worker)))))
          ;; Add the buffer-local hook now that we know a section exists
          (add-hook 'kill-buffer-hook #'magit-gptcommit--buffer-kill-hook nil t))
        ;; move section to correct position
        (oset magit-root-section children
              (magit-gptcommit--move-last-to-position
               (oref magit-root-section children) pos))))))

(defun magit-gptcommit--clear-message-history ()
  "Clear the history of worker messages."
  (clrhash magit-gptcommit--last-messages)
  (magit-gptcommit--debug "Cleared message history"))

(defun magit-gptcommit-generate ()
  "Generate gptcommit message and insert it into the buffer."
  (interactive)
  (magit-gptcommit--debug "Starting new gptcommit generation in buffer %s" (current-buffer))
  ;; Clear message history when explicitly generating a new message
  (magit-gptcommit--clear-message-history)
  (when (magit-gptcommit--running-p)
    (magit-gptcommit--debug "Aborting existing gptcommit worker before generating new one")
    (magit-gptcommit-abort))
  (pcase (current-buffer)
    ((app (buffer-local-value 'major-mode) 'magit-status-mode)
     (magit-gptcommit--debug "Removing any existing GPT commit section")
     (magit-gptcommit-remove-section)
     (magit-gptcommit--debug "Inserting new GPT commit section with staged changes")
     (magit-gptcommit--insert 'staged t))
    ((app (buffer-local-value 'major-mode) 'magit-diff-mode)
     (magit-gptcommit--debug "GPT commit in diff mode not implemented yet")) ; TODO
    ((pred (buffer-local-value 'with-editor-mode))
     (magit-gptcommit--debug "GPT commit in with-editor mode not implemented yet")) ; TODO
    (_ (user-error "Not in a magit status buffer or with-editor buffer"))))

(cl-defun magit-gptcommit-abort (&optional (repository (magit-repository-local-repository)))
  "Abort gptcommit process for current REPOSITORY."
  (interactive)
  (when-let ((worker (magit-repository-local-get 'magit-gptcommit--active-worker nil repository)))
    (magit-gptcommit--debug "Aborting worker with key: %s"
                            (magit-gptcommit--worker-key worker))
    ;; Mark worker as inactive first to prevent further callbacks from working
    (setf (magit-gptcommit--worker-active worker) nil)
    (magit-gptcommit--debug "Marked worker as inactive")

    (when (and worker
               (magit-gptcommit--worker-llm-request worker))
      (magit-gptcommit--debug "Cancelling LLM request for worker")
      (condition-case err
          (llm-cancel-request (magit-gptcommit--worker-llm-request worker))
        (error (magit-gptcommit--debug "Error cancelling LLM request: %S" err))))

    ;; Always clean up the repository local variable
    (magit-gptcommit--debug "Removing worker from repository-local storage")
    (magit-repository-local-delete 'magit-gptcommit--active-worker repository)

    ;; Mark request as no longer in progress and update end time
    (setq magit-gptcommit--request-in-progress nil)
    (setq magit-gptcommit--last-request-end-time (float-time))
    (magit-gptcommit--debug "Request marked as aborted and no longer in progress")))

(defun magit-gptcommit-remove-section ()
  "Remove gptcommit SECTION from magit buffer if exist."
  (interactive)
  (when-let ((section (magit-gptcommit--goto-target-section 'gptcommit))
             (inhibit-read-only t))
    (magit-gptcommit--debug "Removing gptcommit section from buffer %s" (current-buffer))
    (with-slots (start end) section
      (magit-gptcommit--debug "Deleting region from %s to %s" start end)
      (delete-region start end))
    (magit-gptcommit--debug "Removing section from magit-root-section children")
    (delete section (oref magit-root-section children))))

(defun magit-gptcommit--process-commit-message (message orig-message)
  "Process commit message based on generated MESSAGE and current commit message ORIG-MESSAGE.

Executed in the context of the commit message buffer."
  ;; Process the message but not the instructions at the end.
  (save-restriction
    (goto-char (point-min))
    (narrow-to-region
     (point)
     (if (re-search-forward (concat "^" comment-start) nil t)
         (max 1 (- (point) 2))
       (point-max)))
    (pcase magit-gptcommit-commit-message-action
      ('append (goto-char (point-max)))
      ('prepend (goto-char (point-min)))
      (_ (delete-region (point-min) (point)))) ; defaults to 'replace
    ;; Insert the new message.
    (insert (string-trim message)
            (or
             (and (eq magit-gptcommit-commit-message-action 'prepend) " ")
             "\n"))))

;;;; Commit Message
(defun magit-gptcommit-commit-accept ()
  "Accept gptcommit message, after saving current message."
  (interactive)
  (when-let ((message (magit-repository-local-get 'magit-gptcommit--last-message))
             (buf (magit-commit-message-buffer)))
    (with-current-buffer buf
      (let (orig-message)
        ;; save the current non-empty and newly written comment,
        ;; because otherwise it would be irreversibly lost.
        (when-let ((message (git-commit-buffer-message)))
          (unless (ring-member log-edit-comment-ring message)
            (ring-insert log-edit-comment-ring message)
            (setq orig-message message)))
        (funcall magit-gptcommit-process-commit-message-function message orig-message)
        (save-buffer)))))

(defun magit-gptcommit-commit-create ()
  "Execute `magit-commit-create' and bring gptcommit message to editor."
  (interactive)
  (let ((hook #'magit-gptcommit-commit-accept))
    (magit-gptcommit--add-hook-run-once 'git-commit-setup-hook hook)
    (magit-commit-create)))

(defun magit-gptcommit-commit-quick ()
  "Accept gptcommit message and make a commit with current staged."
  (interactive)
  (if-let ((message (magit-repository-local-get 'magit-gptcommit--last-message)))
      (magit-run-git "commit" "-m" message)
    (user-error "No last gptcommit message found")))

;;;; response handling

(defun magit-gptcommit--stream-insert-response (msg info)
  "Insert prompt response.

MSG is the response.
INFO is the request metadata."
  (magit-gptcommit--debug "Stream response received: %s..." (substring msg 0 (min 30 (length msg))))
  (when-let* ((worker-buf (plist-get info :buffer))
              (start-position (plist-get info :position))
              (tracking-marker (plist-get info :tracking-marker))
              (worker (magit-repository-local-get 'magit-gptcommit--active-worker)))

    ;; Always store the message in our last-messages table, regardless of worker status
    ;; This helps us keep track of all messages for cases when multiple workers are created/aborted
    (puthash (magit-gptcommit--worker-key worker)
             (cons (magit-gptcommit--worker-creation-time worker) msg)
             magit-gptcommit--last-messages)

    ;; Check if worker is still active
    (if (not (magit-gptcommit--worker-active worker))
        (magit-gptcommit--debug "⚠️ Ignoring response for inactive worker with key: %s"
                                (magit-gptcommit--worker-key worker))

      ;; Worker is active, proceed with insertion
      (magit-gptcommit--debug "Processing response for worker with key: %s"
                              (magit-gptcommit--worker-key worker))
      (condition-case err
          (progn
            ;; Set worker message first to ensure it's available
            (magit-gptcommit--debug "Updating worker message")
            (setf (magit-gptcommit--worker-message worker) msg)

            ;; Process sections if they exist
            (when (magit-gptcommit--worker-sections worker)
              (magit-gptcommit--debug "Worker has %d section(s)"
                                      (length (magit-gptcommit--worker-sections worker)))
              (dolist (pair (magit-gptcommit--worker-sections worker))
                (-let (((buf . section) pair))
                  (magit-gptcommit--debug "Processing section in buffer %s" buf)
                  (when (and section
                             buf
                             (buffer-live-p buf))
                    (magit-gptcommit--debug "Buffer is live, updating content")
                    (with-current-buffer buf
                      (save-excursion
                        (condition-case err2
                            (let ((inhibit-read-only t)
                                  (magit-insert-section--parent magit-root-section))
                              (with-slots (start content end) section
                                ;; Update heading status first
                                (magit-gptcommit--debug "Updating section heading to 'Typing...'")
                                (magit-gptcommit--update-heading-status "Typing..." 'success)

                                ;; Use safe deletion to avoid buffer corruption
                                (magit-gptcommit--debug "Attempting to replace content from %s to %s"
                                                        start-position tracking-marker)
                                (if (magit-gptcommit--safe-delete-region
                                     (marker-position start-position)
                                     (marker-position tracking-marker)
                                     buf)
                                    (progn
                                      ;; Deletion successful, insert new content
                                      ;; Use the helper function for consistent formatting
                                      (magit-gptcommit--insert-message start-position msg buf)
                                      (setq end tracking-marker))
                                  ;; Deletion failed, log warning
                                  (magit-gptcommit--debug "⚠️ Skipped content update due to safety check"))))
                          (error
                           (magit-gptcommit--debug "Error inserting response: %S" err2)
                           (message "Error inserting response: %S" err2))))))))))
        (error
         (magit-gptcommit--debug "Stream response error: %S" err)
         (message "Stream response error: %S" err))))))

(cl-defun magit-gptcommit--stream-update-status (status &optional (error-msg))
  "Update status of gptcommit section.

STATUS is one of `success', `error'.
ERROR-MSG is error message."
  (magit-gptcommit--debug "Updating section status to '%s'" status)
  (when-let* ((worker (magit-repository-local-get 'magit-gptcommit--active-worker))
              (sections (and worker
                             (magit-gptcommit--worker-sections worker))))

    ;; Check if worker is still active before proceeding
    (if (not (magit-gptcommit--worker-active worker))
        (magit-gptcommit--debug "⚠️ Ignoring status update for inactive worker with key: %s"
                              (magit-gptcommit--worker-key worker))

      ;; Worker is active, proceed with update
      (magit-gptcommit--debug "Found worker with key: %s" (magit-gptcommit--worker-key worker))
      (magit-gptcommit--debug "Worker has %d section(s)" (length sections))
      (dolist (pair sections)
        (-let (((buf . section) pair))
          (when (and section
                     buf
                     (buffer-live-p buf))
            (with-current-buffer buf
              (save-excursion
                (condition-case err
                    (let ((inhibit-read-only t)
                          (magit-insert-section--parent magit-root-section))
                      ;; Check if section markers are valid before proceeding
                      (if (and (slot-boundp section 'start)
                               (slot-boundp section 'content)
                               (slot-boundp section 'end)
                               (markerp (oref section start))
                               (markerp (oref section content))
                               (markerp (oref section end))
                               (> (marker-position (oref section start)) 1)
                               (> (marker-position (oref section content))
                                  (marker-position (oref section start))))
                          ;; Section markers are valid
                          (with-slots (start content end) section
                            (pcase status
                              ('success
                               (magit-gptcommit--debug "Setting status to 'Done'")
                               (magit-gptcommit--update-heading-status "Done" 'success)
                               ;; Use the newest message from our collection, if available
                               (let* ((worker (magit-repository-local-get 'magit-gptcommit--active-worker))
                                      (newest-message (magit-gptcommit--find-newest-message))
                                      (last-message (or newest-message
                                                        (and worker
                                                             (magit-gptcommit--worker-message worker))))
                                      (key (and worker
                                                (magit-gptcommit--worker-key worker))))
                                 (when (and last-message key)
                                   (magit-gptcommit--debug "Using%s message for completion"
                                                          (if newest-message " newest" ""))
                                   (magit-repository-local-set 'magit-gptcommit--last-message last-message)
                                   (magit-gptcommit--cache-set key last-message)))
                               ;; update section properties
                               (put-text-property content end 'magit-section section)
                               ;; update keymap
                               (put-text-property content end 'keymap (get-text-property start 'keymap)))
                              ('error
                               (magit-gptcommit--update-heading-status
                                (format "Response Error: %s" error-msg) 'error))))
                        ;; Invalid section markers
                        (magit-gptcommit--debug "⚠️ Invalid section markers in buffer %s" buf)))
                  (error
                   (magit-gptcommit--debug "Error updating section status: %S" err)
                   (message "Error updating section status: %S" err)))))))))))

;;;; llm

(defun magit-gptcommit--llm-provider ()
  "Return llm provider stored in `magit-gptcommit-llm-provider'.

If `magit-gptcommit-llm-provider' is a function, call it without arguments.
Otherwise, return the stored value."
  (if (functionp magit-gptcommit-llm-provider)
      (funcall magit-gptcommit-llm-provider)
    magit-gptcommit-llm-provider))

(defun magit-gptcommit--llm-get-partial-callback (info callback)
  "Return parial callback for llm streaming responses.

INFO is the request metadata.
Calls CALLBACK with the prompt response and INFO to update the response."
  (lambda (response)
    (funcall callback response info)))

(defun magit-gptcommit--llm-get-response-callback (info callback)
  "Return response callback for llm.

INFO is the request metadata.
Calls CALLBACK with the prompt response and INFO to update the response."
  (lambda (response)
    (condition-case err
        (let ((worker (magit-repository-local-get 'magit-gptcommit--active-worker)))
          ;; Only process the response if worker is still active
          (if (and worker (magit-gptcommit--worker-active worker))
              (progn
                (funcall callback response info)
                (when (and (plist-get info :position)
                          (plist-get info :tracking-marker))
                  (let ((start-position (marker-position (plist-get info :position)))
                        (tracking-marker (marker-position (plist-get info :tracking-marker))))
                    (when (and start-position tracking-marker
                              ;; Safety check for marker positions
                              (> start-position 1)
                              (> tracking-marker start-position))
                      (pulse-momentary-highlight-region start-position tracking-marker))))
                (magit-gptcommit--stream-update-status 'success))
            ;; Worker is inactive or gone, ignore response
            (magit-gptcommit--debug "⚠️ Ignoring response callback - worker inactive or gone")))
      (error
       (magit-gptcommit--debug "Error in response callback: %S" err)
       (message "Error in response callback: %S" err)))
    (magit-gptcommit--llm-finalize)))

(defun magit-gptcommit--llm-error-callback (err msg)
  "The error callback for llm."
  (magit-gptcommit--debug "LLM error callback triggered: %s" msg)
  (condition-case err2
      (magit-gptcommit--stream-update-status 'error msg)
    (error
     (magit-gptcommit--debug "Error in stream update status: %S" err2)
     (message "Error in llm error callback: %S" err2)))

  ;; Always ensure the request is properly finalized, even if errors occurred
  (condition-case err3
      (magit-gptcommit--llm-finalize)
    (error
     (magit-gptcommit--debug "Error in llm finalize: %S" err3)
     ;; Manually reset the request-in-progress flag as a last resort
     (setq magit-gptcommit--request-in-progress nil)
     (setq magit-gptcommit--last-request-end-time (float-time)))))

(defun magit-gptcommit--insert-message (pos msg &optional buffer)
  "Insert MSG at position POS in BUFFER or current buffer.
Ensures consistent message formatting across all insertion points."
  (with-current-buffer (or buffer (current-buffer))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char pos)
        (insert msg "\n\n")))))

(defun magit-gptcommit--update-section-with-latest-message ()
  "Update the current gptcommit section with the newest message available.
This ensures that even if the current worker was aborted, the newest message
from any worker is displayed in the section."
  (magit-gptcommit--debug "Attempting to update section with latest message")
  (condition-case err
      (when-let* ((newest-msg (magit-gptcommit--find-newest-message))
                 (section (magit-gptcommit--goto-target-section 'gptcommit)))
        (with-slots (start content end) section
          (when (and (markerp content) (markerp end)
                     (> (marker-position content) 1)
                     (> (marker-position end) (marker-position content)))
            ;; Check current content to avoid unnecessary buffer modifications
            (let* ((current-content (buffer-substring-no-properties
                                     (marker-position content)
                                     (marker-position end)))
                   (current-msg (string-trim current-content))
                   (needs-update (not (string= current-msg newest-msg))))

              (if needs-update
                  (progn
                    (magit-gptcommit--debug "Found valid section with different content, updating")
                    (let ((inhibit-read-only t))
                      ;; Clear any existing content
                      (delete-region content end)
                      ;; Insert newest message using the helper function
                      (magit-gptcommit--insert-message content newest-msg)))
                (magit-gptcommit--debug "Section already contains newest message, skipping update"))))))
    (error
     (magit-gptcommit--debug "Error updating section with latest message: %S" err))))

(defun magit-gptcommit--llm-finalize ()
  "Finalize llm prompt response."
  (magit-gptcommit--debug "Finalizing LLM prompt response, cleaning up worker")
  ;; Try to update the section with the latest message before cleaning up
  (magit-gptcommit--update-section-with-latest-message)
  ;; Clean up the worker
  (magit-repository-local-delete 'magit-gptcommit--active-worker)

  ;; Mark request as no longer in progress and update end time
  (setq magit-gptcommit--request-in-progress nil)
  (setq magit-gptcommit--last-request-end-time (float-time))
  (magit-gptcommit--debug "Request completed, marked as no longer in progress"))

(defun magit-gptcommit--llm-chat-streaming (key info callback)
  "Retrieve response to prompt in INFO.

KEY is a unique identifier for the request.

INFO is a plist with the following keys:
- :prompt (the prompt being sent)
- :buffer (the magit buffer)
- :position (marker at which to insert the response).
- :tracking-marker (a marker that tracks the end of the inserted response text).

Call CALLBACK with the response and INFO with partial and full responses."
  (magit-gptcommit--debug "Request for LLM chat streaming with key: %s" key)

  ;; Check if a request is already in progress or if we're in cooldown period
  (let* ((current-time (float-time))
         (time-since-last-completion (- current-time magit-gptcommit--last-request-end-time))
         (in-cooldown (< time-since-last-completion magit-gptcommit-request-cooldown)))

    (cond
     ;; If a request is already in progress, abort and show message
     (magit-gptcommit--request-in-progress
      (magit-gptcommit--debug "⚠️ Request blocked - another LLM request is in progress")
      (message "Another GPT commit request is already in progress. Please wait...")
      ;; Update section with a waiting message
      (when-let ((section (magit-gptcommit--goto-target-section 'gptcommit)))
        (with-slots (start) section
          (save-excursion
            (goto-char (+ 12 start))
            (delete-region (point) (pos-eol))
            (insert (propertize "Waiting for previous request" 'font-lock-face 'warning))))))

     ;; If we're in cooldown period, wait until cooldown finishes
     (in-cooldown
      (let ((remaining (- magit-gptcommit-request-cooldown time-since-last-completion)))
        (magit-gptcommit--debug "Request in cooldown period (%.2f seconds remaining)" remaining)
        (when-let ((section (magit-gptcommit--goto-target-section 'gptcommit)))
          (with-slots (start) section
            (save-excursion
              (goto-char (+ 12 start))
              (delete-region (point) (pos-eol))
              (insert (propertize (format "Cooling down (%.1fs)" remaining)
                                  'font-lock-face 'warning)))))
        (sit-for remaining)

        ;; After cooldown, proceed with request
        (magit-gptcommit--llm-chat-streaming-request key info callback)))

     ;; Otherwise, process the request
     (t (magit-gptcommit--llm-chat-streaming-request key info callback)))))

(defun magit-gptcommit--llm-chat-streaming-request (key info callback)
  "Actual implementation of the LLM chat streaming request.
See `magit-gptcommit--llm-chat-streaming' for parameter documentation."
  (magit-gptcommit--debug "Starting new LLM chat streaming request with key: %s" key)
  (let* ((prompt (plist-get info :prompt))
         (buffer (plist-get info :buffer))
         (llm-provider (magit-gptcommit--llm-provider))
         (partial-callback
          (magit-gptcommit--llm-get-partial-callback info callback))
         (response-callback
          (magit-gptcommit--llm-get-response-callback info callback))
         (error-callback #'magit-gptcommit--llm-error-callback))

    ;; Mark that a request is now in progress
    (setq magit-gptcommit--request-in-progress t)
    (magit-gptcommit--debug "Request marked as in progress")

    (magit-gptcommit--debug "Creating worker for buffer: %s" buffer)
    (magit-gptcommit--debug "Using prompt length: %d characters" (length prompt))

    (magit-repository-local-set
     'magit-gptcommit--active-worker
     (make-magit-gptcommit--worker
      :key key
      :llm-request (llm-chat-streaming
                    llm-provider
                    (llm-make-chat-prompt
                     prompt
                     :temperature magit-gptcommit-llm-provider-temperature
                     :max-tokens magit-gptcommit-llm-provider-max-tokens)
                    partial-callback
                    response-callback
                    error-callback)))
    (magit-gptcommit--debug "Worker created and stored in repository-local variable")))

;; Add buffer-kill-hooks to abort gptcommit when magit buffers are killed
(defun magit-gptcommit--buffer-kill-hook ()
  "Abort gptcommit when a buffer is killed that might be part of the process."
  (when-let* ((worker (magit-repository-local-get 'magit-gptcommit--active-worker))
              (sections (and worker (magit-gptcommit--worker-sections worker)))
              (current-buf (current-buffer)))
    ;; If this buffer is in the sections, abort the entire process
    (when (assq current-buf sections)
      (magit-gptcommit--debug "Magit buffer killed, aborting gptcommit process")
      (magit-gptcommit-abort))))

;;;; Footer

(provide 'magit-gptcommit)

;;; magit-gptcommit.el ends here
