;;; doctor-chatgpt.el --- Ask ChatGPT in Emacs        -*- lexical-binding: t; -*-

;;; Commentary:

;; Before using this package, you need to install the revChatGPT
;; and set the API key with the `auth-source' package.
;; You can use pip to install the revChatGPT:
;;
;; python -m pip install revChatGPT
;;
;; And you can find more details in the `doctor-chatgpt-api-token'.
;; After that, you can use `doctor-chatgpt' to ask ChatGPT.

;;; Links:

;; https://emacs-china.org/t/chatgpt-emacs-doctor/23773
;; https://github.com/acheong08/ChatGPT

;;; Code:

(require 'markdown-mode)

(defvar doctor-chatgpt-buffer-name "*doctor-chatgpt*")
(defvar doctor-chatgpt-process-buffer-name "*doctor-chatgpt-process*")
(defvar doctor-chatgpt-process nil)
(defvar doctor-chatgpt-replying nil)
(defvar doctor-chatgpt-recv-list nil)
(defvar doctor-chatgpt-send-list nil)

(defcustom doctor-chatgpt-revchatgpt-version "v3"
  "Choose the version of revChatGPT.
See https://github.com/acheong08/ChatGPT#installation"
  :type 'string
  :options '("v1" "v3")
  :group 'doctor-chatgpt)

(defun doctor-chatgpt-revchatgpt-program ()
  "Return the start script of the revChatGPT program."
  (cond
   ((string= doctor-chatgpt-revchatgpt-version "v1")
    '("python" "-m" "revChatGPT.V1"))
   ((string= doctor-chatgpt-revchatgpt-version "v3")
    `("python" "-m" "revChatGPT.V3" "--api_key"
      ,(doctor-chatgpt-api-token)))))

(defun doctor-chatgpt-revchatgpt-user-prompt ()
  "Return the user prompt."
  (cond
   ((string= doctor-chatgpt-revchatgpt-version "v1")
    "\n+You:\n+$")
   ((string= doctor-chatgpt-revchatgpt-version "v3")
    "\n+User: \n+$")))

(defun doctor-chatgpt-revchatgpt-chatgpt-prompt ()
  "Return the ChatGPT prompt."
  (cond
   ((string= doctor-chatgpt-revchatgpt-version "v1")
    "Chatbot: \n+$")
   ((string= doctor-chatgpt-revchatgpt-version "v3")
    "ChatGPT: \n+$")))

(defun doctor-chatgpt-api-token ()
  "Get the API token from the authinfo.
See https://platform.openai.com/account/api-keys"
  (auth-source-pick-first-password :host "openai.com" :user "chatgpt"))

(defun doctor-chatgpt--insert-line (char)
  "Insert a line with CHAR."
  (insert "\n\n")
  (insert
   (propertize
    (make-string 80 char)
    'font-lock-face 'font-lock-comment-face))
  (insert "\n\n"))

(defun doctor-chatgpt--process-filter (_ output)
  "Filter for chatgpt process.
OUTPUT is the string output we need to handle."
  (cond
   ((string-match-p (doctor-chatgpt-revchatgpt-chatgpt-prompt) output)
    (setq doctor-chatgpt-replying t))
   ((string-match-p (doctor-chatgpt-revchatgpt-user-prompt) output)
    (with-current-buffer doctor-chatgpt-buffer-name
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        ;; maybe still have answer output before "User: "
        (when-let* ((_ doctor-chatgpt-replying)
                    (index (string-match (doctor-chatgpt-revchatgpt-user-prompt) output)))
          (insert (substring output 0 index)))
        (doctor-chatgpt--insert-line ?\─))
      (setq doctor-chatgpt-replying nil)
      (read-only-mode 0)))
   (t
    (when doctor-chatgpt-replying ; ignore other output
      (when (> (length output) 1) (push output doctor-chatgpt-recv-list))
      ;; insert answer output to the doctor buffer
      (with-current-buffer doctor-chatgpt-buffer-name
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert output)))))))

(defun doctor-chatgpt--start-process ()
  "Start a chat with ChatGPT."
  (doctor-chatgpt--kill-process)
  (setq doctor-chatgpt-recv-list nil)
  (setq doctor-chatgpt-send-list nil)
  (setq doctor-chatgpt-process
        (let ((process-environment (copy-sequence process-environment)))
          (setenv "NO_COLOR" "true")
          (apply #'start-process
                 `(,doctor-chatgpt-process-buffer-name
                   ,doctor-chatgpt-process-buffer-name
                   ,@(doctor-chatgpt-revchatgpt-program)))))
  (set-process-sentinel doctor-chatgpt-process #'doctor-chatgpt--process-sentinel)
  (set-process-filter doctor-chatgpt-process #'doctor-chatgpt--process-filter))

(defun doctor-chatgpt--process-sentinel (process event)
  "Sentinel for chatgpt process.
PROCESS is the process that changed.
EVENT is a string describing the change."
  (message "%s end with the event %s" process event))

;;;###autoload
(defun doctor-chatgpt-ret-or-read (arg)
  "Insert a newline if preceding character is not a newline.
Otherwise call the Doctor to parse preceding sentence.
ARG will be passed to `newline'."
  (interactive "*p" doctor-chatgpt-mode)
  (if (= (preceding-char) ?\n)
      (doctor-chatgpt-read-print)
    (newline arg)))

;;;###autoload
(defun doctor-chatgpt-read-print ()
  "Top level loop."
  (interactive nil doctor-chatgpt-mode)
  ;; send the sentence before point
  (let ((doctor-sent
         (save-excursion
           (backward-sentence 1)
           (buffer-substring-no-properties (point) (point-max)))))
    (insert "\n")
    (push doctor-sent doctor-chatgpt-send-list)
    (with-current-buffer doctor-chatgpt-buffer-name (read-only-mode 1))
    (process-send-string doctor-chatgpt-process (concat doctor-sent ""))))

;;;###autoload
(defun doctor-chatgpt-restart ()
  "Restart process manually when there is something wrong."
  (interactive)
  (with-current-buffer doctor-chatgpt-buffer-name
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n\n")
      (doctor-chatgpt--insert-line ?\═)
      (insert "Restarting process..."))
    (read-only-mode 0)
    (doctor-chatgpt--start-process)))

;;;###autoload
(defun doctor-chatgpt-exit ()
  "Kill the `doctor-chatgpt-process' with buffers.
`doctor-chatgpt-process-buffer-name' and
`doctor-chatgpt-buffer-name'."
  (interactive)
  (kill-buffer doctor-chatgpt-buffer-name)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer doctor-chatgpt-process-buffer-name)))

(defun doctor-chatgpt--kill-process ()
  "Kill the `doctor-chatgpt-process'."
  (when (and (processp doctor-chatgpt-process)
             (process-live-p doctor-chatgpt-process))
    (kill-process doctor-chatgpt-process)))

(defvar-keymap doctor-chatgpt-mode-map
  "RET" #'doctor-chatgpt-ret-or-read)

(define-derived-mode doctor-chatgpt-mode markdown-mode "Doctor ChatGPT"
  "Major mode for running the ChatGPT.
Like Text mode with Auto Fill mode
except that RET when point is after a newline, or LFD at any time,
reads the sentence before point, and prints the ChatGPT's answer."
  :interactive nil
  (setq-local word-wrap-by-category t)
  (visual-fill-column-mode)
  (insert "Hi. I am the ChatGPT. Please ask me anything, each time you are finished talking, type RET twice."))


;;;###autoload
(defun doctor-chatgpt ()
  "Switch to `doctor-chatgpt-buffer' and start talking with ChatGPT."
  (interactive)
  (doctor-chatgpt--start-process)
  (switch-to-buffer doctor-chatgpt-buffer-name)
  (doctor-chatgpt-mode))

(provide 'doctor-chatgpt)
;;; doctor-chatgpt.el ends here

