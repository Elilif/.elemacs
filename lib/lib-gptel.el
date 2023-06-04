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

(defun eli/gptel--create-prompt (&optional prompt-end)
  "Return a full conversation prompt from the contents of this buffer.

If `gptel--num-messages-to-send' is set, limit to that many
recent exchanges.

If the region is active limit the prompt to the region contents
instead.

If PROMPT-END (a marker) is provided, end the prompt contents
there."
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
						 "[*# \t\n\r-]+"))
				  prompts)
			(and max-entries (cl-decf max-entries))))
		(cons (list :role "system"
					:content (if (and pm (< (point-min) pm))
								 (buffer-substring-no-properties (point-min) (1- pm))
							   gptel--system-message))
			  prompts)))))

(defun eli/gptel-send (&optional arg)
  "Submit this prompt to ChatGPT.

With prefix arg ARG activate a transient menu with more options
instead."
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

(defvar eli/gptel-prompts
  '((translator . (:sys "You are a professional translator."
						:user "You will be provied text delimited by triple backticks, your task is to translate the wrapped text into %s. \n```\n%s\n```"))
	(polish . (:sys "You are an English translator, spelling corrector and improver."
					:user "You will be provied text delimited by triple backticks, your task is to detect the language, translate it and answer in the corrected and improved version of my text, in English. I want you to replace my simplified A0-level words and sentences with more beautiful and elegant, upper level English words and sentences. Keep the meaning same, but make them more literary. I want you to only reply the correction, the improvements and nothing else, do not write explanations. \n```\n%s\n```"))
	(programming . (:sys "You are a professional programmer."
						 :user "You will be provied code delimited by triple backticks, your task is to explain the code to me. \n```\n%s\n```"))
	(summary . (:sys "You are a professional reviewer."
					 :user "You will be provied code delimited by triple backticks, your task is to summarize the wrapped text into a single sentence. \n```\n%s\n```"))
	))

(defun eli/gptel-close ()
  (interactive)
  (let ((frame (selected-frame)))
	(if (frame-parameter frame 'posframe-buffer)
		(posframe--make-frame-invisible frame)
	  (keyboard-quit))))

(defun eli/gptel--do (prompt usr-prompt-get buffer-name width height)
  (if (use-region-p)
	  (let* ((str (buffer-substring-no-properties (region-beginning)
												  (region-end)))
			 (prompts (alist-get prompt eli/gptel-prompts))
			 (gptel--system-message (plist-get prompts :sys))
			 (user-prompt (funcall usr-prompt-get prompts str))
			 (bf-live-p (get-buffer buffer-name))
			 (bf (get-buffer-create buffer-name))
			 (frame (posframe-show bf
								   :position (point)
								   :width width
								   :height height
								   :accept-focus t
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
		  (erase-buffer)
		  (gptel-request user-prompt
						 :stream t)
		  (setq-local cursor-type 'box))
		(deactivate-mark)
		(select-frame-set-input-focus frame))
	(message "Plesae select a region.")))

(defun eli/gptel-translate ()
  (interactive)
  (eli/gptel--do 'translator
				 (lambda (p s)
				   (format (plist-get p :user)
						   (if (string-match "^\\cc" s)
							   "English"
							 "Chinese")
						   s))
				 "*gptel-translator*"
				 60 7))

(defun eli/gptel-polish ()
  (interactive)
  (eli/gptel--do 'polish
				 (lambda (p s)
				   (format (plist-get p :user) s))
				 "*gptel-translator*"
				 60 8))

(defun eli/gptel-program ()
  (interactive)
  (eli/gptel--do 'programming
				 (lambda (p s)
				   (format (plist-get p :user) s))
				 "*gptel-programming*"
				 70 30))

(defun eli/gptel-summary ()
  (interactive)
  (eli/gptel--do 'summary
				 (lambda (p s)
				   (format (plist-get p :user) s))
				 "*gptel-summary*"
				 60 10))


(defun eli/gptel-translate-and-insert ()
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

(defvar eli/gptel-conversations '())

(defun eli/gptel-create-conversation (name)
  (let ((bn (format "*ChatGPT-%s*" name)))
	(add-to-list 'eli/gptel-conversations
				 (cons name bn))
	(gptel bn (gptel--api-key))
	(with-current-buffer bn
	  (setq gptel--num-messages-to-send 5))
	bn))

(defvar eli/gptel--posframe nil)

(defun eli/gptel-posframe-hidehandler (_)
  "Hidehandler used by `eli/gptel-posframe-toggle'."
  (not (eq (selected-frame) eli/gptel--posframe)))

(defun eli/gptel-posframe-toggle (&optional arg)
  "Toggle shell in child frame."
  (interactive "P")
  (unless (and eli/gptel--posframe
			   (frame-live-p eli/gptel--posframe)
			   (not arg))
	(let* ((width  (max 100 (round (* (frame-width) 0.62))))
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
								 :left-fringe 8
								 :right-fringe 8
								 :width width
								 :height height
								 :min-width width
								 :min-height height
								 :internal-border-width 3
								 :background-color (face-background 'tooltip nil t)
								 :override-parameters '((cursor-type . box))
								 :accept-focus t))
	  (with-selected-frame eli/gptel--posframe
		(set-frame-parameter eli/gptel--posframe 'line-spacing 10)
		(setq cursor-type 'box))))
  ;; Focus in child frame
  (select-frame-set-input-focus eli/gptel--posframe)
  (goto-char (point-max)))

(defun eli/gptel-exit ()
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
