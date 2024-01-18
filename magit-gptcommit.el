;;; magit-gptcommit.el --- Git commit with help of gpt -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Tiou Lims

;; Author: Tiou Lims <dourokinga@gmail.com>
;; URL: https://github.com/douo/magit-gptcommit
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (magit "2.90.1") (gpt "0.1.0"))

;;; Commentary:

;; This package provides a way to commit with help of gpt.

;;; Code:

(require 'magit)
(require 'gptel)

;;;###autoload
(define-minor-mode magit-gptcommit-mode
  "Magit gptcommit mode"
  :require 'magit-gptcommit
  :group 'magit-gptcommit
  :global t
  (if magit-gptcommit-mode
      (progn
        (magit-add-section-hook 'magit-status-sections-hook
                                #'magit-gptcommit--insert-gptcommit
                                nil
                                'append)
        )
    ;; Disable mode
    (remove-hook 'magit-status-sections-hook #'magit-gptcommit--insert-gptcommit))
  )

(defvar magit-gptcommit--commit-alist nil
  "Alist of generated commit message.")

(cl-defun magit-gptcommit--target-position (&optional condition)
  "Return end position of section after which to insert the commit message."
  (cl-labels ((find-section (condition)
                (save-excursion
                  (goto-char (point-min))
                  (ignore-errors
                    (cl-loop until (magit-section-match condition)
                             do ((print (magit-current-section))
                                 (magit-section-forward)
                                 )
                             finally return (magit-current-section))))))
    (save-excursion
      (goto-char (point-min))
      (when-let ((section (if (null condition)
                              (or (find-section 'tags)
                                  (find-section 'tag)
                                  (find-section 'branch))
                            (find-section condition)
                            )
                          )
                 )
        ;; Add 1 to leave blank line after top sections.
        (1+ (oref section end)))
      )))

(defun magit-gptcommit--insert-gptcommit ()
  (if-let* ((buf (current-buffer))
            (inhibit-read-only t)
            ;; any running process associated with this buffer?
            (proc-attrs
             (cl-find-if
              (lambda (proc-list) ;; (process . attrs)
                (eq (plist-get (cdr proc-list) :buffer) buf))
              gptel-curl--process-alist))
            (proc (car proc-attrs))
            (reminder "Generating...")
            )
      ;; if yes, then just insert the generated commit message
      (let (
            (magit-insert-section--parent magit-root-section)
            )
        (goto-char (magit-gptcommit--target-position))
        (magit-insert-section ('gptcommit nil nil)
          (magit-insert-heading (format "GPT Commit(%s)" reminder))
          (insert magit-gptcommit--tmp)
          (insert "\n")
          )
        )
    (setq magit-gptcommit--tmp nil)
    (gptel-curl-get-response
     (list :prompt (list (list :role "user" :content "你好，帮我写100个字的文本，每行不超过 20 字符"))
           :buffer buf
           )
     (lambda (msg info)
       (message msg)
       (with-current-buffer buf
         (save-excursion
           (goto-char (magit-gptcommit--target-position))
           (message "match : %s" (magit-section-match 'gptcommit))
           (if-let* ((inhibit-read-only t)
                     (magit-insert-section--parent magit-root-section)
                     ((magit-section-match 'gptcommit)))
               ;; update existing section(current section)
               ;;
               (let ((section (magit-current-section)))
                 (message "update existing section")
                 (setq magit-gptcommit--tmp (concat magit-gptcommit--tmp msg))
                 (goto-char  (1- (oref section end)))
                 ;; update section content
                 (oset section content magit-gptcommit--tmp)
                 (insert msg)
                 )
             ;; insert new section
             (message "insert new section")
             (setq magit-gptcommit--tmp msg)
             (magit-insert-section (gptcommit nil nil)
               (magit-insert-heading "GPT Commit")
               (insert magit-gptcommit--tmp)
               (insert "\n")
               )
             )
           ))
       )
     )
    )
  )

;;;; OpenAI


(defconst magit-gptcommit--common-args
  (if (memq system-type '(windows-nt ms-dos))
      '("--disable" "--location" "--silent" "-XPOST"
        "-y300" "-Y1" "-D-")
    '("--disable" "--location" "--silent" "--compressed"
      "-XPOST" "-y300" "-Y1" "-D-"))
  "Arguments always passed to Curl for gptel queries.")

(cl-defmethod magit-gptcommit--request-data (prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (let ((prompts-plist
         `(:model "gpt-3.5-turbo"
                  :messages [,@prompts]
                  ))
        prompts-plist))

  (defcustom magit-gptcommit--api-key "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"
    "An API key (string) for the default LLM backend.

OpenAI by default.

Can also be a function of no arguments that returns an API
key (more secure) for the active backend."
    :group 'magit-gptcommit
    :type '(choice
            (string :tag "API key")
            (function :tag "Function that returns the API key")))

  (defun magit-gptcommit--get-args (prompts token)
    "Produce list of arguments for calling Curl.

PROMPTS is the data to send, TOKEN is a unique identifier."
    (let* ((url "https://api.openai.com/v1/chat/completions")
           (data (encode-coding-string
                  (json-encode (gptel--request-data prompts))
                  'utf-8))
           (headers
            (append '(("Content-Type" . "application/json"))
                    `("Authorization" . ,(concat "Bearer " magit-gptcommit--api-key))
                    )))
      (append
       gptel-curl--common-args
       (list (format "-w(%s . %%{size_header})" token))
       (list (format "-d%s" data))
       (cl-loop for (key . val) in headers
                collect (format "-H%s: %s" key val))
       (list url))))

  (defvar magit-gptcommit--process-alist nil
    "Alist of active GPTel curl requests.")

  ;; "--disable" "--location" "--silent" "--compressed" "-XPOST" "-y300" "-Y1" "-D-" "-w(b247fffa6f4b995afd30035679d2a09a . %{size_header})" "-d{\"model\":\"gpt-3.5-turbo\",\"messages\":[{\"role\":\"system\",\"content\":\"You are a large language model living in Emacs and a helpful assistant. Respond concisely.\"},{\"role\":\"user\",\"content\":\"Ni\345\245\275\"}],\"stream\":true,\"temperature\":1.0}" "-HContent-Type: application/json" "-HAuthorization: Bearer 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" "https://api.openai.com/v1/chat/completions"
  (defun magit-gptcommit--get-response (info &optional callback)
    "Retrieve response from OpenAI API."
    (let* ((token (md5 (format "%s%s%s%s"
                               (random) (emacs-pid) (user-full-name)
                               (recent-keys)))
                  )
           (args (magit-gptcommit--get-args (plist-get info :prompt) token))
           (process (start-process "magit-gptcommit" (generate-new-buffer "*magit-gptcommit*") "curl" args)))
      (set-process-sentinel process
                            (lambda (process event)
                              (if (string= event "finished\n")
                                  (progn
                                    (message "Process %s finished" process)
                                    (kill-buffer (process-buffer process))
                                    )
                                (message "Process %s failed" process))))
      (set-process-filter process
                          ;; gptel-curl--stream-filter
                          (lambda (process output)
                            (with-current-buffer (process-buffer process)
                              (save-excursion
                                (goto-char (process-mark process))
                                (insert output)
                                (set-marker (process-mark process) (point)))
                              (message "Process %s output: %s" process (magit-gptcommit--parse-stream)))))
      )
    )

  (cl-defun magit-gptcommit--parse-stream ()
    (let* ((json-object-type 'plist)      ;
           (content-strs))
      (condition-case nil
          (while (re-search-forward "^data:" nil t)
            (save-match-data
              (unless (looking-at " *\\[DONE\\]")
                (when-let* ((response (json-read))
                            (delta (map-nested-elt
                                    response '(:choices 0 :delta)))
                            (content (plist-get delta :content)))
                  (push content content-strs)))))
        (error
         (goto-char (match-beginning 0))))
      (apply #'concat (nreverse content-strs))))

;;; magit-gptcommit.el ends here
