;; lib-vterm.el --- Initialize lib-vterm configurations.	-*- lexical-binding: t; -*-

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
  (require 'posframe)
  (require 'vterm))

;;; src: https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-shell.el#L201
(defvar shell-pop--frame nil)

(defun shell-pop-posframe-hidehandler (_)
  "Hidehandler used by `shell-pop-posframe-toggle'."
  (not (eq (selected-frame) shell-pop--frame)))

;;;###autoload
(defun shell-pop-posframe-toggle ()
  "Toggle shell in child frame."
  (interactive)
  (let ((dir (shell-quote-argument
			  (expand-file-name default-directory))))
	;; Shell pop in child frame
	(unless (and shell-pop--frame
				 (frame-live-p shell-pop--frame))
	  (let ((width  (max 100 (round (* (frame-width) 0.62))))
			(height (round (* (frame-height) 0.62)))
			(buffer (vterm--internal (lambda (_arg) t))))
		(setq shell-pop--frame
			  (posframe-show
			   buffer
			   :poshandler #'posframe-poshandler-frame-center
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
		  (setq-local cursor-type 'box) ; blink cursor
		  (goto-char (point-max))
		  (when (fboundp 'vterm-reset-cursor-point)
			(vterm-reset-cursor-point)))))
	;; Focus in child frame
	(select-frame-set-input-focus shell-pop--frame)
	(unless (string= dir (shell-quote-argument
						  (expand-file-name default-directory)))
	  (vterm-send-string (concat "cd " dir))
	  (vterm-send-return))))

;;;###autoload
(defun eli/vterm-quit ()
  (interactive)
  (let ((frame (selected-frame)))
	(if (frame-parameter frame 'posframe-buffer)
		(posframe--make-frame-invisible frame)
	  (keyboard-quit))))


;;;; provide
(provide 'lib-vterm)
;;; lib-vterm.el ends here.
