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
  key llm-request message sections)

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
  (if (not magit-gptcommit-llm-provider)
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
             (oldkey (and worker (oref worker key))))
        (if-let ((msg (and (not no-cache) (magit-gptcommit--cache-get key))))
            ;; cache hit
            (magit-insert-section (gptcommit nil nil)
              (magit-insert-heading
                (format
                 (propertize "GPT commit: %s" 'font-lock-face 'magit-section-heading)
                 (propertize "Cache" 'font-lock-face 'success)))
              (insert msg "\n\n")
              (magit-repository-local-set 'magit-gptcommit--last-message msg))
          (magit-insert-section (gptcommit nil nil)
            (magit-insert-heading
              (format
               (propertize "GPT commit: %s" 'font-lock-face 'magit-section-heading)
               (propertize "Waiting" 'font-lock-face 'warning)))
            (if (and worker (equal key oldkey))
                ;; if yes, then just insert the generated commit message
                (progn
                  ;; FIXME: Attemp to clean old section from buffer but not working
                  ;; So we have to set `magit-inhibit-refresh' to avoid the problem.
                  (assq-delete-all buf (oref worker sections))
                  (insert (oref (magit-repository-local-get 'magit-gptcommit--active-worker) message)
                          "\n"))
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
            (oset worker sections
                  (cons (cons buf section)
                        (oref worker sections))))
          (setq-local magit-inhibit-refresh t)
          ;; Add the buffer-local hook now that we know a section exists
          (add-hook 'kill-buffer-hook #'magit-gptcommit--buffer-kill-hook nil t)))
      ;; move section to correct position
      (oset magit-root-section children
            (magit-gptcommit--move-last-to-position
             (oref magit-root-section children) pos)))))


(defun magit-gptcommit-generate ()
  "Generate gptcommit message and insert it into the buffer."
  (interactive)
  (when (magit-gptcommit--running-p)
    (magit-gptcommit-abort))
  (pcase (current-buffer)
    ((app (buffer-local-value 'major-mode) 'magit-status-mode)
     (magit-gptcommit-remove-section)
     (magit-gptcommit--insert 'staged t))
    ((app (buffer-local-value 'major-mode) 'magit-diff-mode)) ; TODO
    ((pred (buffer-local-value 'with-editor-mode))) ; TODO
    (_ (user-error "Not in a magit status buffer or with-editor buffer"))))

(cl-defun magit-gptcommit-abort (&optional (repository (magit-repository-local-repository)))
  "Abort gptcommit process for current REPOSITORY."
  (interactive)
  (when-let ((worker (magit-repository-local-get 'magit-gptcommit--active-worker nil repository)))
    (dolist (pair (oref worker sections))
      (let ((buf (car pair)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (setq-local magit-inhibit-refresh nil)))))
    (when-let ((request (oref worker llm-request)))
      (ignore-errors (llm-cancel-request request)))
    ;; Always clean up the repository local variable
    (magit-repository-local-delete 'magit-gptcommit--active-worker repository)))

(defun magit-gptcommit-remove-section ()
  "Remove gptcommit SECTION from magit buffer if exist."
  (interactive)
  (when-let ((section (magit-gptcommit--goto-target-section 'gptcommit))
             (inhibit-read-only t))
    (with-slots (start end) section
      (delete-region start end))
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
  (let* ((worker-buf (plist-get info :buffer))
         (start-position (plist-get info :position))
         (tracking-marker (plist-get info :tracking-marker))
         (worker (magit-repository-local-get 'magit-gptcommit--active-worker))
         (tmp-message (oref worker message))
         (sections (oref worker sections)))
    (oset worker message msg)
    (dolist (pair sections)
      (-let (((buf . section) pair))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (save-excursion
              (let ((inhibit-read-only t)
                    (magit-insert-section--parent magit-root-section))
                (with-slots (start content end) section
                  (magit-gptcommit--update-heading-status "Typing..." 'success)
                  (delete-region start-position tracking-marker)
                  (goto-char start-position)
                  (insert (format "%s\n\n" msg))
                  (setq end tracking-marker))))))))))

(cl-defun magit-gptcommit--stream-update-status (status &optional (error-msg))
  "Update status of gptcommit section.

STATUS is one of `success', `error'.
ERROR-MSG is error message."
  ;; (message "magit-gptcommit--stream-update-status %s" status)
  (when-let* ((worker (magit-repository-local-get 'magit-gptcommit--active-worker))
              (sections (and worker (oref worker sections))))
    (dolist (pair sections)
      (-let (((buf . section) pair))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (save-excursion
              (let ((inhibit-read-only t)
                    (magit-insert-section--parent magit-root-section))
                (with-slots (start content end) section
                  (pcase status
                    ('success
                     (magit-gptcommit--update-heading-status "Done" 'success)
                     (let* ((worker (magit-repository-local-get 'magit-gptcommit--active-worker))
                            (last-message (and worker (oref worker message)))
                            (key (and worker (oref worker key))))
                       (when (and last-message key)
                         (magit-repository-local-set 'magit-gptcommit--last-message last-message)
                         (magit-gptcommit--cache-set key last-message)))
                     ;; update section properties
                     (put-text-property content end 'magit-section section)
                     ;; update keymap
                     (put-text-property content end 'keymap (get-text-property start 'keymap)))
                    ('error
                     (magit-gptcommit--update-heading-status (format "Response Error: %s" error-msg) 'error))))))))))))

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
    (condition-case nil
        (progn
          (funcall callback response info)
          (let ((start-position (marker-position (plist-get info :position)))
                (tracking-marker (marker-position (plist-get info :tracking-marker))))
            (pulse-momentary-highlight-region start-position tracking-marker))
          (magit-gptcommit--stream-update-status 'success)))
    (magit-gptcommit--llm-finalize)))

(defun magit-gptcommit--llm-error-callback (err msg)
  "The error callback for llm."
  (condition-case nil
      (magit-gptcommit--stream-update-status 'error msg))
  (magit-gptcommit--llm-finalize))

(defun magit-gptcommit--llm-finalize ()
  "Finalize llm prompt response."
  (setq-local magit-inhibit-refresh nil)
  (magit-repository-local-delete 'magit-gptcommit--active-worker))

(defun magit-gptcommit--llm-chat-streaming (key info callback)
  "Retrieve response to prompt in INFO.

KEY is a unique identifier for the request.

INFO is a plist with the following keys:
- :prompt (the prompt being sent)
- :buffer (the magit buffer)
- :position (marker at which to insert the response).
- :tracking-marker (a marker that tracks the end of the inserted response text).

Call CALLBACK with the response and INFO with partial and full responses."
  (let* ((prompt (plist-get info :prompt))
         (buffer (plist-get info :buffer))
         (llm-provider (magit-gptcommit--llm-provider))
         (partial-callback
          (magit-gptcommit--llm-get-partial-callback info callback))
         (response-callback
          (magit-gptcommit--llm-get-response-callback info callback))
         (error-callback #'magit-gptcommit--llm-error-callback))

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
                    error-callback)))))

;; Add buffer-kill-hooks to abort gptcommit when magit buffers are killed
(defun magit-gptcommit--buffer-kill-hook ()
  "Abort gptcommit when a buffer is killed that might be part of the process."
  (when-let* ((worker (magit-repository-local-get 'magit-gptcommit--active-worker))
              (sections (and worker (oref worker sections)))
              (current-buf (current-buffer)))
    ;; If this buffer is in the sections, abort the entire process
    (when (assq current-buf sections)
      (message "Magit buffer killed, aborting gptcommit process")
      (magit-gptcommit-abort))))

;;;; Footer

(provide 'magit-gptcommit)

;;; magit-gptcommit.el ends here
