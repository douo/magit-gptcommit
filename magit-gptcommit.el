;;; magit-gptcommit.el --- Git commit with help of gpt -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Tiou Lims

;; Author: Tiou Lims <dourokinga@gmail.com>
;; URL: https://github.com/douo/magit-gptcommit
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (dash "2.13.0") (magit "2.90.1") (llm "0.14.1"))

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
  "Structure respesenting current active process."
  key llm-request message sections)

(defconst magit-gptcommit-prompt-one-line "You are an expert programmer writing a commit message.
You went over every file diff that was changed in it.

First Determine the best label for the diffs.
Here are the labels you can choose from:
- build: Changes that affect the build system or external dependencies (example scopes: gulp, broccoli, npm)
- chore: Updating libraries, copyrights or other repo setting, includes updating dependencies.
- ci: Changes to our CI configuration files and scripts (example scopes: Travis, Circle, GitHub Actions)
- docs: Non-code changes, such as fixing typos or adding new documentation
- feat: a commit of the type feat introduces a new feature to the codebase
- fix: A commit of the type fix patches a bug in your codebase
- perf: A code change that improves performance
- refactor: A code change that neither fixes a bug nor adds a feature
- style: Changes that do not affect the meaning of the code (white-space, formatting, missing semi-colons, etc)
- test: Adding missing tests or correcting existing tests

Then summarize the commit into a single specific and cohesive theme.
Remember to write in only one line, no more than 50 characters.
Write your response using the imperative tense following the kernel git commit style guide.
Write a high level title.

THE FILE DIFFS:
```
%s
```
Now write Commit message in follow template: [label]:[one line of summary] :
")

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

;;; Cache
(defvar magit-gptcommit-cache-limit 30
  "Max number of cache entries.")

(defvar magit-gptcommit--cache nil
  "Cache of generated commit message.")

(cl-defun magit-gptcommit--cache-key (content &optional (repository (magit-repository-local-repository)))
  "Return cache key for CONTENT and REPOSITORY."
  ;; set repository to default if not initial
  (md5 (format "%s%s" repository content)))

(defun magit-gptcommit--cache-set (key value)
  "Set cache VALUE for KEY."
  (let ((cache magit-gptcommit--cache))
    (if cache
        (let ((keyvalue (assoc key cache)))
          (if keyvalue
              ;; Update pre-existing value for key.
              (setcdr keyvalue value)
            ;; No such key in repository-local cache.
            ;; if cache is full, remove half of it
            (when (>= (length cache) magit-gptcommit-cache-limit)
              (setf cache
                    (seq-take cache (/ (length cache) 2))))
            ;; Add new key-value pair to cache.
            (push (cons key value) cache)))
      ;; No cache
      (push (cons key value)
            cache))))

(defun magit-gptcommit--cache-get (key &optional default)
  "Return cache value for KEY or DEFAULT if not found."
  (if-let ((keyvalue (magit-gptcommit--cache-p key)))
      (cdr keyvalue) ; TODO: LRU Cache
    default))

(defun magit-gptcommit--cache-p (key)
  "Non-nil when a value exists for KEY.

Return a (KEY . VALUE) cons cell.

The KEY is matched using `equal'."
  (and-let* ((cache magit-gptcommit--cache))
    (assoc key cache)))


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


(defun magit-gptcommit--retrieve-stashed-diff ()
  "Retrieve stashed diff.
assuming current section is staged section."
  (let* ((section (magit-current-section))
         ;; HACK:  1 token ~= 4 chars
         (max-char (- (* 4 magit-gptcommit-max-token) (length magit-gptcommit-prompt)))
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
  (save-excursion
    (when-let ((buf (current-buffer))
               ;; 存在 staged 才自动生成
               ;; TODO: magit-anything-staged-p
               (pos (magit-gptcommit--goto-target-position condition))
               (inhibit-read-only t)
               (magit-insert-section--parent magit-root-section))

      (magit-repository-local-delete 'magit-gptcommit--last-message)
      (let* ((diff (magit-gptcommit--retrieve-stashed-diff))
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
              (insert msg)
              (insert "\n")
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
                  (insert (oref (magit-repository-local-get 'magit-gptcommit--active-worker) message))
                  (insert "\n"))
              (when worker
                (magit-gptcommit-abort))
              (insert "\n")
              (magit-gptcommit--llm-chat-streaming
               key
               (list :prompt (format magit-gptcommit-prompt diff)
                     :buffer buf
                     :position (point-marker))
               #'magit-gptcommit--stream-insert-response)))
          ;; store section in repository-local active worker
          (let ((section (car (last (oref magit-root-section children))))
                (worker (magit-repository-local-get 'magit-gptcommit--active-worker)))
            (oset worker sections
                  (cons (cons buf section)
                        (oref worker sections))))
          (setq-local magit-inhibit-refresh t)))
      ;; move section to correct position
      (oset magit-root-section children
            (magit-gptcommit--move-last-to-position
             (oref magit-root-section children) pos)))))


(defun magit-gptcommit-generate ()
  "Generate gptcommit message and insert it into the buffer."
  (interactive)
  (when (magit-gptcommit--running-p)
    (magit-gptcommit-abort))
  (magit-gptcommit-remove-section)
  (pcase (current-buffer)
    ((app (buffer-local-value 'major-mode) 'magit-status-mode)
     (magit-gptcommit--insert 'staged t))
    ((app (buffer-local-value 'major-mode) 'magit-diff-mode)) ; TODO
    ((pred (buffer-local-value 'with-editor-mode))) ; TODO
    (_ (user-error "Not in a magit status buffer or with-editor buffer"))))

(cl-defun magit-gptcommit-abort (&optional (repository (magit-repository-local-repository)))
  "Abort gptcommit process for current REPOSITORY."
  (interactive)
  (when-let ((worker (magit-repository-local-get 'magit-gptcommit--active-worker nil repository)))
    (message "%s" worker)
    (dolist (pair (oref worker sections))
      (let ((buf (car pair)))
        (with-current-buffer buf
          (setq-local magit-inhibit-refresh nil))))
    (magit-repository-local-delete 'magit-gptcommit--active-worker repository)
    (llm-cancel-request (oref worker llm-request))))

(defun magit-gptcommit-remove-section ()
  "Remove gptcommit SECTION from magit buffer if exist."
  (interactive)
  (when-let ((section (magit-gptcommit--goto-target-section 'gptcommit))
             (inhibit-read-only t))
    (with-slots (start end) section
      (delete-region start end))
    (delete section (oref magit-root-section children))))

;;;; Commit Message
(defun magit-gptcommit-commit-accept ()
  "Accept gptcommit message, after saving current message."
  (interactive)
  (when-let ((message (magit-repository-local-get 'magit-gptcommit--last-message))
             (buf (magit-commit-message-buffer)))
    (with-current-buffer buf
      ;; save the current non-empty and newly written comment,
      ;; because otherwise it would be irreversibly lost.
      (when-let ((message (git-commit-buffer-message)))
        (unless (ring-member log-edit-comment-ring message)
          (ring-insert log-edit-comment-ring message)))
      ;; Delete the message but not the instructions at the end.
      (save-restriction
        (goto-char (point-min))
        (narrow-to-region
         (point)
         (if (re-search-forward (concat "^" comment-start) nil t)
             (max 1 (- (point) 2))
           (point-max)))
        (delete-region (point-min) (point)))
      ;; Insert the new message.
      (insert message))))

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
  "Insert response in target section located by CONDITION.
MSG the response
INFO is request metadata"
  (let* ((worker-buf (plist-get info :buffer))
         (tracking-marker (plist-get info :tracking-marker)) ; dupilicate with section end
         (worker (magit-repository-local-get 'magit-gptcommit--active-worker))
         (tmp-message (oref worker message))
         (sections (oref worker sections)))
    (oset worker message (concat tmp-message msg))
    (dolist (pair sections)
      (-let (((buf . section) pair))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (save-excursion
              (let ((inhibit-read-only t)
                    (magit-insert-section--parent magit-root-section))
                (with-slots (start content end) section
                  ;; update heading
                  (magit-gptcommit--update-heading-status "Typing..." 'success)
                  (goto-char  (1- end)) ;; before \newline
                  (insert msg)
                  (when (eq worker-buf buf)
                    (setq tracking-marker end)
                    ;; (set-marker-insertion-type tracking-marker t)
                    (plist-put info :tracking-marker tracking-marker)))))))))))

(cl-defun magit-gptcommit--stream-update-status (status &optional (error-msg))
  "Update status of gptcommit section.

STATUS is one of `success', `error'
INFO is request metadata
ERROR-MSG is error message"
  ;; (message "magit-gptcommit--stream-update-status %s" status)
  (let* ((worker (magit-repository-local-get 'magit-gptcommit--active-worker))
        (sections (oref worker sections)))
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
                            (last-message (oref worker message))
                            (key (oref worker key)))
                       (magit-repository-local-set 'magit-gptcommit--last-message last-message)
                       (magit-gptcommit--cache-set key last-message))
                     ;; update section properties
                     (put-text-property content end 'magit-section section)
                     ;; update keymap
                     (put-text-property content end 'keymap (get-text-property start 'keymap)))
                    ('error
                     (magit-gptcommit--update-heading-status (format "Response Error: %s" error-msg) 'error))))))))))))

;;;; llm

(defun magit-gptcommit--llm-provider ()
  (if (functionp magit-gptcommit-llm-provider)
      (funcall magit-gptcommit-llm-provider)
    magit-gptcommit-llm-provider))

(defun magit-gptcommit--llm-chat-finalize-callback (info)
  (setq-local magit-inhibit-refresh nil)
  (magit-repository-local-delete 'magit-gptcommit--active-worker))

(defun magit-gptcommit--llm-chat-streaming (key info callback)
  (let* ((prompt (plist-get info :prompt))
         (buffer (plist-get info :buffer))
         (llm-provider (magit-gptcommit--llm-provider))
         (partial-callback 'ignore)
         (response-callback
          (lambda (response)
            (condition-case nil
                (progn
                  (funcall callback response info)
                  (magit-gptcommit--stream-update-status 'success)
                  (let ((start-position (plist-get info :position))
                        (tracking-marker (plist-get info :tracking-marker)))
                    (pulse-momentary-highlight-region start-position tracking-marker))))
            (magit-gptcommit--llm-chat-finalize-callback info)))
         (error-callback
          (lambda (err)
            (condition-case nil
                (progn
                  ;; (message (format "ERROR-CALLBACK: %s" err))
                  (magit-gptcommit--stream-update-status 'error err)))
            (magit-gptcommit--llm-chat-finalize-callback info))))

    (magit-repository-local-set
     'magit-gptcommit--active-worker
     (make-magit-gptcommit--worker
      :key key
      :llm-request (llm-chat-streaming
                    llm-provider
                    (make-llm-chat-prompt
                     :interactions
                     (list (make-llm-chat-prompt-interaction
                            :role 'user :content prompt))
                     :temperature magit-gptcommit-llm-provider-temperature)
                    partial-callback
                    response-callback
                    error-callback)))))

;;;; Footer

(provide 'magit-gptcommit)

;;; magit-gptcommit.el ends here
