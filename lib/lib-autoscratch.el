;; lib-autoscratch.el --- Initialize lib-autoscratch configurations.	-*- lexical-binding: t; -*-

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

(defun eli/autoscratch--fork-and-rename-current ()
  "Rename buffer and create new autoscratch.
If `autoscratch-fork-after-trigger' is t, create a
new autoscratch buffer and rename the current one
to $mode-scratch."
  (when (eq t autoscratch-fork-after-trigger)
	(let* ((name (format "*%s-scratch*"
						 (replace-regexp-in-string
						  "-mode" ""
						  (format "%s" major-mode))))
		   (buf (get-buffer-create name))
		   content)
	  (rename-buffer (generate-new-buffer-name "*temp*"))
	  (with-current-buffer buf
		(setq content (buffer-substring (point-min)
										(point-max)))
		(erase-buffer)
		(rename-buffer "*scratch*")
		(when initial-scratch-message
          (insert (substitute-command-keys initial-scratch-message))
          (set-buffer-modified-p nil))
		(autoscratch-mode))
	  (save-excursion
		(goto-char (point-min))
		(insert content)
		(unless (bobp)
		  (insert "\n\n")))
	  (rename-buffer name)
	  (when (functionp 'sp-remove-active-pair-overlay)
		(sp-remove-active-pair-overlay))
	  (when popper-mode
		(popper--find-buried-popups)
		;; (setq popper-open-popup-alist
		;; 	  (list (cons (get-buffer-window)
		;; 				  (get-buffer (buffer-name)))))
		)
	  (setq autoscratch-trigger-after (point)))))

(defun eli/autoscratch--yank (&optional arg)
  "Look for autoscratch trigger, execute if found and call `yank'.

ARG is the content of the clipboard being yanked."
  (interactive "*P")
  (save-excursion
	(yank arg))
  (autoscratch--look-for-triggers t)
  (goto-char (point-max)))

(defun eli/popper-remove-autoscratch (group)
  (popper--update-popups)
  (let ((buried-popups (cdr (assoc group popper-buried-popup-alist))))
	(setf (alist-get group popper-buried-popup-alist
					 nil nil 'equal)
		  (cl-remove-if (lambda (buf)
						  (string-match "-scratch" (buffer-name buf)))
						buried-popups :key #'cdr))))


;;;; provide
(provide 'lib-autoscratch)
;;; lib-autoscratch.el ends here.
