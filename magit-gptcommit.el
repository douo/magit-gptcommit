;;; magit-gptcommit.el --- Git commit with help of gpt -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Tiou Lims

;; Author: Tiou Lims <dourokinga@gmail.com>
;; URL: https://github.com/douo/magit-gptcommit
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (magit "2.90.1") (gpt "0.1.0"))

;;; Commentary:

;; This package provides a way to commit with help of gpt.

;;; Code:

;;;; Requirements

(require 'magit)
(require 'gptel-curl)


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
                                'append)
        )
    ;; Disable mode
    (remove-hook 'magit-status-sections-hook #'magit-gptcommit--status-insert-gptcommit))
  )

(defvar magit-gptcommit--last-message nil
  "GPT generated commit message for current repository.")
(defvar magit-gptcommit--active-section-list nil
  "List of active gptcommit sections for current repository.")
(defvar magit-gptcommit--active-process nil
  "Running gptcommit process for current repository.
Stored as a cons cell (PROCESS . RESPONSE) where RESPONE is a SSO Message.")

(defconst magit-gptcommit--prompt-one-line "You are an expert programmer writing a commit message.
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

(defcustom magit-gptcommit--prompt magit-gptcommit--prompt-one-line
  "The prompt that was used to generate the commit message."
  :type 'string
  :group 'magit-gptcommit)

(defun magit-gptcommit-load-prompt ()
  (let* ((directory (expand-file-name "assets/magit-gptcommit/" user-emacs-directory))
         (file-path (expand-file-name "prompt.txt" directory))
         (url "https://example.com/prompt.txt"))
    (unless (file-exists-p directory)
      (make-directory directory t))
    (if (file-exists-p file-path)
        (with-temp-buffer
          (insert-file-contents file-path)
          (setq magit-gptcommit--prompt (buffer-string)))
      ;; TODO download from url
      (url-retrieve url (lambda (status)
p                          (when (equal (car status) :ok)
                            (with-temp-buffer (url-retrieve-sentinel (current-buffer))
                                              (write-region (point-min) (point-max) file-path)
                                              (setq magit-gptcommit--prompt (buffer-string)))))))))

(defcustom magit-gptcommit--max-token 4096
  "Max token length."
  :type 'integer
  :group 'magit-gptcommit)

(defvar magit-gptcommit--commit-alist nil
  "Alist of generated commit message.")

(cl-defun magit-gptcommit--move-last-to-position (list position)
  "Move the last element of LIST to POSITION."
  (append (take position list) (last list) (butlast (-drop position list))))

(cl-defun magit-gptcommit--goto-target-position (&optional (condition '(tags tag branch))) ;;
  "Return end position of section after which to insert the commit message.
Position is determined by CONDITION, which is defined in `magit-section-match'."
  (let ((children (oref magit-root-section children))
        (pos 0)
        )
    ;; iterate over all children
    (cl-loop for child in children
             ;; find the first child that matches the condition
             if (or (null condition)
                    (magit-section-match condition child))
             return (goto-char (identity (oref child start)))
             do (cl-incf pos)  ;; index after target section
             )
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
                          return child
                          ))
    (when target
      (goto-char (oref target start))
      target)))

(cl-defun magit-gptcommit--cache-key (content &optional (repository (magit-repository-local-repository)))
  "Return cache key for CONTENT and REPOSITORY."
  ;; set repository to default if not initial
  (md5 (format "%s%s" repository content)))

(defun magit-gptcommit--retrieve-stashed-diff ()
  "Retrieve stashed diff.
assuming current section is staged section."
  (let* ((section (magit-current-section))
         ;; HACK:  1 token ~= 4 chars
         (max-char (- (* 4 magit-gptcommit--max-token) (length magit-gptcommit--prompt)))
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
    ;; any running process associated with this buffer?
  ;; (proc-attrs
  ;;  (cl-find-if
  ;;   (lambda (proc-list) ;; (process . attrs)
  ;;     (eq (plist-get (cdr proc-list) :buffer) buf))
  ;;   gptel-curl--process-alist))
  ;; (proc (car proc-attrs))
  (magit-repository-local-exists-p 'magit-gptcommit--active-process repository)
  )
(cl-defun magit-gptcommit--abort (&optional (repository (magit-repository-local-repository)))
  "Abort gptcommit process for current REPOSITORY."
  (magit-repository-local-delete 'magit-gptcommit--active-process repository)
  (gptel-abort (current-buffer))
  )

(defun magit-gptcommit--status-insert-gptcommit ()
  "Insert gptcommit section into status buffer."
  (save-excursion
    (when-let ((buf (current-buffer))
               ;; 存在 staged 才自动生成
               ;; TODO: magit-anything-staged-p
               (pos (magit-gptcommit--goto-target-position 'staged))
               (inhibit-read-only t)
               (magit-insert-section--parent magit-root-section))
      (magit-insert-section (gptcommit nil nil)
        (magit-insert-heading
          (format
           (propertize "GPT commit: %s" 'font-lock-face 'magit-section-heading)
           (propertize "Waiting" 'font-lock-face 'warning)))
        (if (magit-gptcommit--running-p)
            ;; if yes, then just insert the generated commit message
            (progn  (insert (cdr (magit-repository-local-get 'magit-gptcommit--active-process)))
                    (insert "\n"))
          (insert "\n")
          (let ((diff (magit-gptcommit--retrieve-stashed-diff)))
            (magit-repository-local-delete 'magit-gptcommit--last-message)
            (magit-gptcommit-gptel-get-response
             (list :prompt (list (list :role "user" :content (format magit-gptcommit--prompt diff)))
                   :buffer buf
                   :position (point-marker))
             (apply-partially #'magit-gptcommit--stream-insert-response 'staged)))))
      ;; move section to correct position
      (oset magit-root-section children
            (magit-gptcommit--move-last-to-position
             (oref magit-root-section children) pos)))))

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


(defun magit-gptcommit-commit-quick ()
  "Accept gptcommit message and make a commit with current staged."
  (interactive)
  (when-let ((message (magit-repository-local-get 'magit-gptcommit--last-message)))
    (magit-run-git "commit" "-m" message)
    ))

;;;; gptel

;;;;; modified from `gptel-curl-get-response'
(defun magit-gptcommit-gptel-get-response (info callback)
  "Retrieve response to prompt in INFO.

INFO is a plist with the following keys:
- :prompt (the prompt being sent)
- :buffer (the gptel buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards.  If omitted
the response is inserted into the current buffer after point."
  (let* ((token (md5 (format "%s%s%s%s"
                             (random) (emacs-pid) (user-full-name)
                             (recent-keys))))
         (args (gptel-curl--get-args (plist-get info :prompt) token))
         (stream (and gptel-stream (gptel-backend-stream gptel-backend)))
         (process (apply #'start-process "gptel-curl"
                         (generate-new-buffer "*gptel-curl*") "curl" args)))
    (when gptel--debug
      (message "%S" args))
    ;; store process in repository-local variable
    (magit-repository-local-set 'magit-gptcommit--active-process
                                (cons process nil))
    (with-current-buffer (process-buffer process)
      (set-process-query-on-exit-flag process nil)
      (setf (alist-get process gptel-curl--process-alist)
            (nconc (list :token token
                         ;; FIXME `aref' breaks `cl-struct' abstraction boundary
                         ;; FIXME `cl--generic-method' is an internal `cl-struct'
                         :parser (cl--generic-method-function
                                  (if stream
                                      ;; 调用 buffer 对应 backend 的 stream parser
                                      (cl-find-method
                                       'gptel-curl--parse-stream nil
                                       (list
                                        (aref (buffer-local-value
                                               'gptel-backend (plist-get info :buffer))
                                              0) t))
                                    (cl-find-method
                                     'gptel--parse-response nil
                                     (list
                                      ;; Emacs 中的 array 是字符串或 vector [1 2 3]
                                      (aref (buffer-local-value
                                             'gptel-backend (plist-get info :buffer))
                                            0) t t))))
                         :callback (or callback
                                       (if stream
                                           #'gptel-curl--stream-insert-response
                                         #'gptel--insert-response))
                         :transformer nil)
                   info))
      (if stream
          (progn (set-process-sentinel process #'magit-gptcommit--stream-cleanup)
                 (set-process-filter process #'magit-gptcommit--stream-filter))
        (set-process-sentinel process #'gptel-curl--sentinel)))))

(defun magit-gptcommit--stream-cleanup (process _status)
  "Process sentinel for GPTel curl requests.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (when gptel--debug
      (with-current-buffer proc-buf
        (clone-buffer "*gptel-error*" 'show)))
    (let* ((info (alist-get process gptel-curl--process-alist))
           (gptel-buffer (plist-get info :buffer))
           (backend-name
            (gptel-backend-name
             (buffer-local-value 'gptel-backend gptel-buffer)))
           (tracking-marker (plist-get info :tracking-marker))
           (start-marker (plist-get info :position))
           (http-status (plist-get info :http-status))
           (http-msg (plist-get info :status))
           (callback (plist-get info :callback))
           response-beg response-end)
      (if (equal http-status "200")
          (progn
            ;; Finish handling response
            (with-current-buffer (marker-buffer start-marker)
              (setq response-beg (+ start-marker 2)
                    response-end (marker-position tracking-marker))
              (pulse-momentary-highlight-region response-beg tracking-marker)
              ;; (when gptel-mode (save-excursion (goto-char tracking-marker)
              ;;                                  (insert "\n\n" (gptel-prompt-prefix-string))))
              )
            (with-current-buffer gptel-buffer
              (magit-gptcommit--stream-update-status 'success info)
              ))
        ;; Or Capture error message
        (with-current-buffer proc-buf
          (goto-char (point-max))
          (search-backward (plist-get info :token))
          (backward-char)
          (pcase-let* ((`(,_ . ,header-size) (read (current-buffer)))
                       (json-object-type 'plist)
                       (response (progn (goto-char header-size)
                                        (condition-case nil (json-read)
                                          (json-readtable-error 'json-read-error))))
                       (error-data (plist-get response :error)))
            (cond
             (error-data
              (if (stringp error-data)
                  (message "%s error: (%s) %s" backend-name http-msg error-data)
                (when-let ((error-msg (plist-get error-data :message)))
                  (message "%s error: (%s) %s" backend-name http-msg error-msg))
                (when-let ((error-type (plist-get error-data :type)))
                  (setq http-msg (concat "("  http-msg ") " (string-trim error-type))))))
             ((eq response 'json-read-error)
              (message "ChatGPT error (%s): Malformed JSON in response." http-msg))
             (t (message "ChatGPT error (%s): Could not parse HTTP response." http-msg)))))
        (with-current-buffer gptel-buffer
          ;; tell callback error occurred
          (magit-gptcommit--stream-update-status 'error info http-msg)
          ))
      (with-current-buffer gptel-buffer
        (run-hook-with-args 'gptel-post-response-functions response-beg response-end)))
    (setf (alist-get process gptel-curl--process-alist nil 'remove) nil)
    ;; clear repository-local variable
    (magit-repository-local-delete 'magit-gptcommit--active-process)
    (kill-buffer proc-buf)))

(defun magit-gptcommit--stream-filter (process output)
  (let* ((proc-info (alist-get process gptel-curl--process-alist)))
    (with-current-buffer (process-buffer process)
      ;; Insert output
      (save-excursion
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point)))

      ;; Find HTTP status
      (unless (plist-get proc-info :http-status)
        (save-excursion
          (goto-char (point-min))
          (when-let* (((not (= (line-end-position) (point-max))))
                      (http-msg (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                      (http-status
                       (save-match-data
                         (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                              (match-string 1 http-msg)))))
            (plist-put proc-info :http-status http-status)
            (plist-put proc-info :status (string-trim http-msg))))
        ;; Handle read-only gptel buffer
        ;; (when (with-current-buffer (plist-get proc-info :buffer)
        ;;         (or buffer-read-only
        ;;             (get-char-property (plist-get proc-info :position) 'read-only)))
        ;;   (message "Buffer is read only, displaying reply in buffer \"*ChatGPT response*\"")
        ;;   (display-buffer
        ;;    (with-current-buffer (get-buffer-create "*ChatGPT response*")
        ;;      (goto-char (point-max))
        ;;      (move-marker (plist-get proc-info :position) (point) (current-buffer))
        ;;      (current-buffer))
        ;;    '((display-buffer-reuse-window
        ;;       display-buffer-pop-up-window)
        ;;      (reusable-frames . visible))))
        ;; Run pre-response hook
        (when (and (equal (plist-get proc-info :http-status) "200")
                   gptel-pre-response-hook)
          (with-current-buffer (marker-buffer (plist-get proc-info :position))
            (run-hooks 'gptel-pre-response-hook))))

      (when-let ((http-msg (plist-get proc-info :status))
                 (http-status (plist-get proc-info :http-status)))
        ;; Find data chunk(s) and run callback
        (when-let (((equal http-status "200"))
                   (response (funcall (plist-get proc-info :parser) nil proc-info))
                   ((not (equal response ""))))
          (funcall (or (plist-get proc-info :callback)
                       #'gptel-curl--stream-insert-response)
                   response proc-info)
          (magit-gptcommit--stream-update-status 'typing proc-info)
          )))))

(defun magit-gptcommit--stream-insert-response (condition msg info)
  "Insert response in target section located by CONDITION.
MSG the response
 INFO metadata"
  (let ((buf (plist-get info :buffer))
        (tracking-marker (plist-get info :tracking-marker)) ;; dupilicate with section end
        )
    (with-current-buffer buf  ;; the buffer who start the gptcommit
      (save-excursion
        (let* ((inhibit-read-only t)
              (magit-insert-section--parent magit-root-section)
              (process-pair (magit-repository-local-get 'magit-gptcommit--active-process))
              (tmp-message (cdr process-pair))
              )
          ;; if gptcommit already existed
          (when-let (section (magit-gptcommit--goto-target-section 'gptcommit))
              ;; update existing section(current section)
              ;; TODO: 不一定是 marker
              (with-slots (start content end) section
                ;; update heading
                (goto-char (+ 12 start))
                (delete-region (point) (point-at-eol))
                (insert (propertize "Typing..." 'font-lock-face 'success))
                ;; (message "update existing section: %s" msg)
                (setcdr process-pair (concat tmp-message msg))
                (goto-char  (1- end)) ;; before \newline
                (insert msg)
                (setq tracking-marker end)
                ;; (set-marker-insertion-type tracking-marker t)
                (plist-put info :tracking-marker tracking-marker)
                )))))))

(cl-defun magit-gptcommit--stream-update-status (status info &optional (msg))
  "Update status of gptcommit section.
STATUS is one of 'success, 'error, 'typing
INFO metadata"
  (message "magit-gptcommit--stream-update-status %s" status)
  (let ((buf (plist-get info :buffer))
        (tracking-marker (plist-get info :tracking-marker)) ;; dupilicate with section end
        )
    (with-current-buffer buf
      (save-excursion
        (let ((inhibit-read-only t)
              (magit-insert-section--parent magit-root-section))
          (when-let ((section (magit-gptcommit--goto-target-section 'gptcommit)))
            (with-slots (start content end) section ;; TODO 不一定是 marker
              (pcase status
                ('success
                 (magit-gptcommit--update-heading-status "Done" 'success)
                 (magit-repository-local-set 'magit-gptcommit--last-message
                                             (cdr (magit-repository-local-get 'magit-gptcommit--active-process)))
                 ;; update section properties
                 (put-text-property content end 'magit-section section)
                 ;; update keymap
                 (put-text-property content end 'keymap (get-text-property start 'keymap)))
                ('error
                 (magit-gptcommit--update-heading-status (format "Response Error: %s" msg) 'error))))))))))

;;;; utils
(defmacro magit-gptcommit--update-heading-status (status face)
  `(progn
    (goto-char (+ 12 start))
    (delete-region (point) (point-at-eol))
    (insert (propertize ,status 'font-lock-face ,face))
    )
  )


;;; magit-gptcommit.el ends here
