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

(defvar-local doctor-chatgpt-buffer-name nil)
(defvar-local doctor-chatgpt-process-buffer-name nil)
(defvar-local doctor-chatgpt-process nil)
(defvar doctor-chatgpt-conversations '())
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
    (make-string 100 char)
    'font-lock-face 'font-lock-comment-face))
  (insert "\n\n"))

;;; TODO: remove unused code and refactor
;; (defun doctor-chatgpt--process-filter (_ output)
;;   "Filter for chatgpt process.
;; OUTPUT is the string output we need to handle."
;;   (cond
;;    ((string-match-p (doctor-chatgpt-revchatgpt-chatgpt-prompt) output)
;;     (setq doctor-chatgpt-replying t))
;;    ((string-match-p (doctor-chatgpt-revchatgpt-user-prompt) output)
;;     (with-current-buffer doctor-chatgpt-buffer-name
;;       (let ((inhibit-read-only t))
;;         (goto-char (point-max))
;;         ;; maybe still have answer output before "User: "
;;         (when-let* ((_ doctor-chatgpt-replying)
;;                     (index (string-match (doctor-chatgpt-revchatgpt-user-prompt) output)))
;;           (insert (substring output 0 index)))
;;         (doctor-chatgpt--insert-line ?\─))
;;       (setq doctor-chatgpt-replying nil)
;;       (read-only-mode 0)))
;;    (t
;;     (when doctor-chatgpt-replying ; ignore other output
;;       (when (> (length output) 1) (push output doctor-chatgpt-recv-list))
;;       ;; insert answer output to the doctor buffer
;;       (with-current-buffer doctor-chatgpt-buffer-name
;;         (let ((inhibit-read-only t))
;;           (goto-char (point-max))
;;           (insert output)))))))

;; (defun doctor-chatgpt--start-process ()
;;   "Start a chat with ChatGPT."
;;   (doctor-chatgpt--kill-process)
;;   (setq doctor-chatgpt-recv-list nil)
;;   (setq doctor-chatgpt-send-list nil)
;;   (setq doctor-chatgpt-process
;;         (let ((process-environment (copy-sequence process-environment)))
;;           (setenv "NO_COLOR" "true")
;;           (apply #'start-process
;;                  `(,doctor-chatgpt-process-buffer-name
;;                    ,doctor-chatgpt-process-buffer-name
;;                    ,@(doctor-chatgpt-revchatgpt-program)))))
;;   (set-process-sentinel doctor-chatgpt-process #'doctor-chatgpt--process-sentinel)
;;   (set-process-filter doctor-chatgpt-process #'doctor-chatgpt--process-filter))

(defun doctor-chatgpt--process-filter (process output)
  "Filter for chatgpt process.
OUTPUT is the string output we need to handle."
  (let ((buffer (string-replace "-process" "" (process-name process))))
	(cond
	 ((string-match-p (doctor-chatgpt-revchatgpt-chatgpt-prompt) output)
	  (setq doctor-chatgpt-replying t))
	 ((string-match-p (doctor-chatgpt-revchatgpt-user-prompt) output)
	  (with-current-buffer buffer
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
		(with-current-buffer buffer
		  (let ((inhibit-read-only t))
			(goto-char (point-max))
			(insert output))))))))

(defun doctor-chatgpt-process-set (name)
  (setq doctor-chatgpt-recv-list nil)
  (setq doctor-chatgpt-send-list nil)
  (let* ((process-name (concat "*doctor-chatgpt-process-" name "*"))
		 (buffer-name (concat "*doctor-chatgpt-" name "*")))
	(get-buffer-create buffer-name)
	(add-to-list 'doctor-chatgpt-conversations
				 (cons name buffer-name))
	(with-current-buffer buffer-name
	  (setq doctor-chatgpt-buffer-name buffer-name)
	  (put 'doctor-chatgpt-buffer-name 'permanent-local t)
	  (setq doctor-chatgpt-process
			(let ((process-environment (copy-sequence process-environment)))
			  (setenv "NO_COLOR" "true")
			  (apply #'start-process
					 `(,process-name
					   ,process-name
					   ,@(doctor-chatgpt-revchatgpt-program)))))
	  (set-process-sentinel doctor-chatgpt-process #'doctor-chatgpt--process-sentinel)
	  (set-process-filter doctor-chatgpt-process #'doctor-chatgpt--process-filter)
	  (put 'doctor-chatgpt-process 'permanent-local t)
	  (setq doctor-chatgpt-process-buffer-name process-name)
	  (put 'doctor-chatgpt-process-buffer-name 'permanent-local t)
	  (doctor-chatgpt-mode))
	buffer-name))

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

;; ;;;###autoload
;; (defun doctor-chatgpt-restart ()
;;   "Restart process manually when there is something wrong."
;;   (interactive)
;;   (with-current-buffer doctor-chatgpt-buffer-name
;;     (let ((inhibit-read-only t))
;;       (goto-char (point-max))
;;       (insert "\n\n")
;;       (doctor-chatgpt--insert-line ?\═)
;;       (insert "Restarting process..."))
;;     (read-only-mode 0)
;; 	;;; TODO: write a restart function
;; 	(doctor-chatgpt-process-set)
;;     ;; (doctor-chatgpt--start-process)
;; 	))

;;;###autoload
(defun doctor-chatgpt-exit ()
  "Kill the `doctor-chatgpt-process' with buffers.
`doctor-chatgpt-process-buffer-name' and
`doctor-chatgpt-buffer-name'."
  (interactive)
  (let ((kill-buffer-query-functions nil)
		(buffer (or doctor-chatgpt-buffer-name
					(elemacs-completing-read "Select a conversation: "
											 doctor-chatgpt-conversations))))
	(with-current-buffer buffer
	  (kill-buffer (string-replace "chatgpt" "chatgpt-process" buffer))
	  (setq doctor-chatgpt-conversations
			(cl-remove-if (lambda (cons)
							(string= (cdr cons)
									 doctor-chatgpt-buffer-name))
						  doctor-chatgpt-conversations))
	  (kill-buffer doctor-chatgpt-buffer-name))))
;; (defun doctor-chatgpt-exit ()
;;   "Kill the `doctor-chatgpt-process' with buffers.
;; `doctor-chatgpt-process-buffer-name' and
;; `doctor-chatgpt-buffer-name'."
;;   (interactive)
;;   (let ((kill-buffer-query-functions nil))
;;     (kill-buffer doctor-chatgpt-process-buffer-name)
;; 	(kill-buffer doctor-chatgpt-buffer-name)))

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
  ;;; TODO: use doctor-chatgpt-mode-hook
  (visual-fill-column-mode)
  (mixed-pitch-mode)
  (insert "Hi. I am the ChatGPT. Please ask me anything, each time you are finished talking, type RET twice."))


;;;###autoload
;; (defun doctor-chatgpt ()
;;   "Switch to `doctor-chatgpt-buffer' and start talking with ChatGPT."
;;   (interactive)
;;   (let* ((doctor-chatgpt-buffer-name (doctor-chatgpt-process-set)))
;; 	(switch-to-buffer doctor-chatgpt-buffer-name)))
;; (defun doctor-chatgpt ()
;;   "Switch to `doctor-chatgpt-buffer' and start talking with ChatGPT."
;;   (interactive)
;;   (doctor-chatgpt--start-process)
;;   (switch-to-buffer doctor-chatgpt-buffer-name)
;;   (doctor-chatgpt-mode))

;;;; posframe integration
;;; TODO: design a general framwork to use toggle posframe
(defvar doctor-chatgpt-pop--frame nil)

;;;###autoload
(defun doctor-chatgpt-pop-posframe-toggle ()
  "Toggle shell in child frame."
  (interactive)
  ;; Shell pop in child frame
  (unless (and doctor-chatgpt-pop--frame
			   (frame-live-p doctor-chatgpt-pop--frame)
			   (process-live-p doctor-chatgpt-process))
	(let* ((width  (max 100 (round (* (frame-width) 0.62))))
		   (height (round (* (frame-height) 0.62)))
		   (buffer-name-or-name (elemacs-completing-read "Select a conversation: "
														 doctor-chatgpt-conversations))
		   (buffer (if (rassoc buffer-name-or-name doctor-chatgpt-conversations)
					   buffer-name-or-name
					 (doctor-chatgpt-process-set buffer-name-or-name))))
	  (setq doctor-chatgpt-pop--frame
			(posframe-show
			 buffer
			 :poshandler #'posframe-poshandler-frame-center
			 :hidehandler nil
			 :left-fringe 8
			 :right-fringe 8
			 :width width
			 :height height
			 :min-width width
			 :min-height height
			 :internal-border-width 3
			 :background-color (face-background 'tooltip nil t)
			 :override-parameters '((cursor-type . t))
			 :accept-focus t))

	  (with-current-buffer buffer
		(set-frame-parameter doctor-chatgpt-pop--frame 'line-spacing 10)
		(goto-char (point-max)))))

  ;; Focus in child frame
  (select-frame-set-input-focus doctor-chatgpt-pop--frame)
  (setq-local cursor-type 'box)
  (goto-char (point-max)))

;;;###autoload
(defun eli/doctor-chatgpt-quit ()
  (interactive)
  (let ((frame (selected-frame)))
	(if (frame-parameter frame 'posframe-buffer)
		(progn
		  (posframe--make-frame-invisible frame)
		  (when current-prefix-arg
			(doctor-chatgpt-exit)))
	  (keyboard-quit))))


;;;; prompt
(defcustom doctor-chatgpt-prompts
  '(("English Translator and Improver" . "I want you to act as an English translator, spelling corrector and improver. I will speak to you in any language and you will detect the language, translate it and answer in the corrected and improved version of my text, in English. I want you to replace my simplified A0-level words and sentences with more beautiful and elegant, upper level English words and sentences. Keep the meaning same, but make them more literary. I want you to only reply the correction, the improvements and nothing else, do not write explanations.")
	("Feynman" . "Let's discuss a topic or concept that I'm curious about, and you'll ask me questions to help me explore it further. We'll work together to build a deep understanding of the topic, and you'll provide feedback to help me identify any misconceptions or gaps in my understanding, sort of like the Feynman technique. We'll approach this with an open mind, and we'll be curious and inquisitive as we explore the topic.

I want you to keep in mind that you do also ask specific questions that will push my understanding of said topic, it doesn't matter if I'm not capable of answering cause my goal is to learn more and more. Let's begin.")
	("Feynman_CN" . "在接下来的对话中，我将采用费曼技巧来对一个主题进行阐述，你将扮演一个提问者，针对所有我说的话，理解其中的意思，并且尝试找出里面的漏洞，每次提出一个相应的问题。")
	("Feynman_CN_go_on" . "基于以上信息继续向我提问"))
  "Promps used by doctor-chatgpt"
  :type 'alist
  :group 'doctor-chatgpt)

(defun doctor-chatgpt-insert-prompt (prompt)
  "Insert PROMPT."
  (interactive (list (elemacs-completing-read "Prompts: " doctor-chatgpt-prompts)))
  (insert prompt))

(provide 'doctor-chatgpt)
;;; doctor-chatgpt.el ends here

