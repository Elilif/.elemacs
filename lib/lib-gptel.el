;; lib-gptel.el --- Initialize lib-gptel configurations.	-*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 by Eli

;; Author: Eli <eli.q.qian@gmail.com>
;; URL: https://github.com/Elilif/.elemacs

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;;
;;

;;; Code:

(cl-eval-when (compile)
  (require 'gptel-transient)
  (require 'posframe))

(defvar eli/gptel-prompts
  '((translator . (:sys "You are a professional translator."
						:user "You will be provied with text delimited by triple backticks, your task is to translate the wrapped text into %s. \n```\n%s\n```"))
	(classical . (:sys "你是一位古汉语学者，能熟练的翻译古汉语。"
					   :user "你将收到由三个反引号包裹的古汉语，你的任务是按照以下格式提供它的拼音和释义：\n【拼音】...\n【释义】...\n。\n```\n%s\n```"))
	(polish . (:sys "You are an English translator, spelling corrector and improver."
					:user "You will be provied with text delimited by triple backticks, your task is to detect the language, translate it and answer in the corrected and improved version of my text, in English. I want you to replace my simplified A0-level words and sentences with more beautiful and elegant, upper level English words and sentences. Keep the meaning same, but make them more literary. I want you to only reply the correction, the improvements and nothing else, do not write explanations. \n```\n%s\n```"))
	(programming . (:sys "You are a professional programmer."
						 :user "You will be provied with code delimited by triple backticks, your task is to determine the language and explain the code to me. \n```\n%s\n```"))
	(summary . (:sys "You are a professional reviewer."
					 :user "You will be provied with text delimited by triple backticks, your task is to summarize the wrapped text into a single sentence. \n```\n%s\n```"))))

(defvar eli/gptel-conversations '())
(defvar eli/gptel--posframe nil)
(defvar eli/gptel--last-posframe nil)

(defun eli/gptel--create-prompt (&optional prompt-end)
  "Advice for `gptel--create-prompt'."
  (save-excursion
	(save-restriction
	  (if (use-region-p)
		  (progn (narrow-to-region (region-beginning) (region-end))
				 (goto-char (point-max)))
		(goto-char (or prompt-end (point-max))))
	  (let ((max-entries (and gptel--num-messages-to-send
							  (* 2 (gptel--numberize
									gptel--num-messages-to-send))))
			(pm (when gptel-mode
				  (save-excursion
					(goto-char (point-min))
					(- (search-forward (gptel-prompt-string))
					   (length (gptel-prompt-string))))))
			(prop) (prompts))
		(save-restriction
		  (when pm
			(narrow-to-region pm (point-max)))
		  (while (and
				  (or (not max-entries) (>= max-entries 0))
				  (setq prop (text-property-search-backward
							  'gptel 'response
							  (when (get-char-property (max (point-min) (1- (point)))
													   'gptel)
								t))))
			(push (list :role (if (prop-match-value prop) "assistant" "user")
						:content
						(string-trim
						 (buffer-substring-no-properties (prop-match-beginning prop)
														 (prop-match-end prop))
						 "[*# \t\n\r-]+"
						 "[*# \t\n\r-]+"))
				  prompts)
			(and max-entries (cl-decf max-entries))))
		(cons (list :role "system"
					:content (if (and pm (< (point-min) pm))
								 (buffer-substring-no-properties (point-min) (1- pm))
							   gptel--system-message))
			  prompts)))))

;;;###autoload
(defun eli/gptel-send (&optional arg)
  "Advice for `gptel-send'."
  (interactive "P")
  (if (and arg (require 'gptel-transient nil t))
      (call-interactively #'gptel-menu)
	(message "Querying ChatGPT...")
	(let ((leng (length (gptel-prompt-string))))
	  (when (>  leng 30)
		(insert "\n")
		(insert (string-limit (gptel-prompt-string) (/ leng 2)))))
	(let* ((response-pt
			(if (use-region-p)
				(set-marker (make-marker) (region-end))
              (point-marker)))
           (gptel-buffer (current-buffer))
           (full-prompt (gptel--create-prompt response-pt)))
      (funcall
       (if gptel-use-curl
           #'gptel-curl-get-response #'gptel--url-get-response)
       (list :prompt full-prompt
			 :buffer gptel-buffer
			 :position response-pt)))
    (gptel--update-header-line " Waiting..." 'warning)))

;;;###autoload
(defun eli/gptel-close ()
  "Close current gptel posframe."
  (interactive)
  (let ((frame (selected-frame)))
	(if (frame-parameter frame 'posframe-buffer)
		(posframe--make-frame-invisible frame)
	  (keyboard-quit))))

(defun eli/gptel-query-get-from-region ()
  "Get query string from selected region."
  (if (or (use-region-p)
		  pdf-view-active-region)
	  (cond
	   ((eq major-mode 'pdf-view-mode)
		(car (pdf-view-active-region-text)))
	   (t
		(buffer-substring-no-properties (region-beginning)
										(region-end))))
	(error "Plesae select a region.")))

(defun eli/gptel-query-get-from-minibuffer ()
  "Get query string from minibuffer."
  (read-string "Input: "))

(defun eli/gptel-usr-prompt-get-defualt (p s)
  (format (plist-get p :user) s))

(cl-defun eli/gptel--do (&key (query-get #'eli/gptel-query-get-from-region)
							  prompt
							  (usr-prompt-get #' eli/gptel-usr-prompt-get-defualt)
							  buffer-name
							  (width 60) (height 7))
  "Create a gptel posframe for PROMPT.

QUERY-GET is a function used to get query. The available functions now are:
`eli/gptel-query-get-from-region', `eli/gptel-query-get-from-minibuffer'.

PROMPT is a symbol, see `eli/gptel-prompts' for more details.

USR-PROMPT-GET is a function of two arguments, called with PROMPTS and CONTENT.
PROMPTS is a plist in `eli/gptel-prompts', CONTENT is the string you select.

BUFFER-NAME is the gptel posframe's name.

WIDTH and HEIGHT specifies the size of posframe, see `posframe-show'
for more details."
  (let* ((str (funcall query-get))
		 (prompts (alist-get prompt eli/gptel-prompts))
		 (gptel--system-message (plist-get prompts :sys))
		 (user-prompt (funcall usr-prompt-get prompts str))
		 (bf-live-p (get-buffer buffer-name))
		 (bf (get-buffer-create buffer-name))
		 (frame (posframe-show bf
							   :position (point)
							   :width width
							   :height height
							   :border-width 2
							   :border-color "light gray"
							   :accept-focus t
							   :initialize (lambda () (erase-buffer))
							   :background-color (face-background 'tooltip nil t))))
	(with-current-buffer bf
	  (unless bf-live-p
		(org-mode)
		(gptel-mode)
		(setq-local gptel-prompt-prefix-alist
					`((markdown-mode . "### ")
					  (org-mode . ,(concat (make-string width ?\-) "\n"))
					  (text-mode . "### ")))
		(set-frame-parameter frame 'line-spacing 10))
	  (setq-local cursor-type 'box))
	(deactivate-mark)
	(select-frame-set-input-focus frame)
	(setq posframe--initialized-p nil
		  eli/gptel--last-posframe frame)
	(gptel-request user-prompt
				   :stream t)))

;;;###autoload
(defun eli/gptel-translate (&optional arg)
  "English-Chinese Translation.

Prefixed with one C-u, Read a string from the minibuffer."
  (interactive "P")
  (eli/gptel--do :query-get (if arg 
								#'eli/gptel-query-get-from-minibuffer
							  #'eli/gptel-query-get-from-region)
				 :prompt 'translator
				 :usr-prompt-get (lambda (p s)
								   (format (plist-get p :user)
										   (if (string-match "^\\cc" s)
											   "English"
											 "Chinese")
										   s))
				 :buffer-name "*gptel-translator*"))

;;;###autoload
(defun eli/gptel-translate-cc (&optional arg)
  "Translate Classical Chinese.

Prefixed with one C-u, Read a string from the minibuffer."
  (interactive "P")
  (eli/gptel--do :query-get (if arg 
								#'eli/gptel-query-get-from-minibuffer
							  #'eli/gptel-query-get-from-region)
				 :prompt 'classical
				 :buffer-name "*gptel-translator*"))

;;;###autoload
(defun eli/gptel-polish ()
  "Polish selected text."
  (interactive)
  (eli/gptel--do :prompt 'polish
				 :buffer-name "*gptel-translator*"
				 :height 8))

;;;###autoload
(defun eli/gptel-program ()
  "Explain selected code."
  (interactive)
  (eli/gptel--do :prompt 'programming
				 :buffer-name "*gptel-programming*"
				 :width 70
				 :height 30))

;;;###autoload
(defun eli/gptel-summary (&optional arg)
  "Summary selected text.

Prefixed with one C-u, select the whole buffer first. That's useful when
reading RSS."
  (interactive "P")
  (eli/gptel--do :query-get (if arg
								(lambda ()
								  (buffer-substring-no-properties
								   (point-min) (point-max)))
							  #'eli/gptel-query-get-from-region)
				 :prompt 'summary
				 :buffer-name "*gptel-summary*"
				 :height 10))

;;;###autoload
(defun eli/gptel-toggle-last-posframe ()
  "Toggle last gptel posframe."
  (interactive)
  (select-frame-set-input-focus eli/gptel--last-posframe))

;;;###autoload
(defun eli/gptel-translate-and-insert ()
  "Translate and replace selected text."
  (interactive)
  (if (use-region-p)
	  (let* ((str (buffer-substring-no-properties (region-beginning)
												  (region-end)))
			 (lang (if (string-match "^\\cc" str)
					   "English"
					 "Chinese"))
			 (prompts (alist-get 'translator eli/gptel-prompts))
			 (gptel--system-message (plist-get prompts :sys))
			 (user-prompt (format (plist-get prompts :user) lang str)))
		(kill-region (region-beginning) (region-end))
		(message "Original text saved to kill-ring.")
		(gptel-request user-prompt
					   :stream t
					   :in-place t))
	(message "Plesae select a sentence.")))

(defun eli/gptel-create-conversation (name)
  "Start ChatGPT session with NAME."
  (let ((bn (format "*ChatGPT-%s*" name)))
	(add-to-list 'eli/gptel-conversations
				 (cons name bn))
	(gptel bn (gptel--api-key))
	(with-current-buffer bn
	  (setq gptel--num-messages-to-send 5))
	bn))

(defun eli/gptel-posframe-hidehandler (_)
  "Hidehandler used by `eli/gptel-posframe-toggle'."
  (not (eq (selected-frame) eli/gptel--posframe)))

;;;###autoload
(defun eli/gptel-posframe-toggle (&optional arg)
  "Pop up last used gptel posframe.
Prefixed with one C-u, select a conversation in `eli/gptel-conversations' or
create one."
  (interactive "P")
  (unless (and eli/gptel--posframe
			   (frame-live-p eli/gptel--posframe)
			   (not arg)
			   (eq (floor (/ (frame-width eli/gptel--posframe) 0.62))
				   (frame-width)))
	(let* ((width  (round (* (frame-width) 0.62)))
		   (height (round (* (frame-height) 0.62)))
		   (buffer-name-or-name (elemacs-completing-read "Select a conversation: "
														 eli/gptel-conversations))
		   (buffer (if (rassoc buffer-name-or-name eli/gptel-conversations)
					   buffer-name-or-name
					 (eli/gptel-create-conversation buffer-name-or-name))))
	  (setq eli/gptel--posframe (posframe-show
								 buffer
								 :poshandler #'posframe-poshandler-frame-center
								 :hidehandler #'eli/gptel-posframe-hidehandler
								 :width width
								 :height height
								 :border-width 2
								 :border-color "light gray"
								 :background-color (face-background 'tooltip nil t)
								 :override-parameters '((cursor-type . box))
								 :accept-focus t))
	  (with-selected-frame eli/gptel--posframe
		(set-frame-parameter eli/gptel--posframe 'line-spacing 10)
		(setq cursor-type 'box))))
  ;; Focus in child frame
  (select-frame-set-input-focus eli/gptel--posframe)
  (goto-char (point-max)))

;;;###autoload
(defun eli/gptel-exit ()
  "Close a ChatGPT session."
  (interactive)
  (let ((kill-buffer-query-functions nil)
		(buffer (elemacs-completing-read "Select a conversation: "
										 eli/gptel-conversations)))
	(kill-buffer buffer)
	(setq eli/gptel-conversations
		  (cl-remove-if (lambda (cons)
						  (string= (cdr cons)
								   buffer))
						eli/gptel-conversations))))


;;;; provide
(provide 'lib-gptel)
;;; lib-gptel.el ends here.
