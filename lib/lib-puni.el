;; lib-puni.el --- Initialize lib-puni configurations.	-*- lexical-binding: t; -*-

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

;; the following code are taken from `expand-region'
;;;###autoload
(defun er/mark-word ()
  "Mark the entire word around or in front of point."
  (interactive)
  (let ((word-regexp "\\sw"))
    (when (or (looking-at word-regexp)
              (looking-back word-regexp (line-beginning-position)))
      (skip-syntax-forward "w")
      (set-mark (point))
      (skip-syntax-backward "w"))))

;;;###autoload
(defun er/mark-symbol ()
  "Mark the entire symbol around or in front of point."
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw"))
	(when (or (looking-at symbol-regexp)
			  (looking-back symbol-regexp (line-beginning-position)))
	  (skip-syntax-forward "_w")
	  (set-mark (point))
	  (skip-syntax-backward "_w"))))

;;;###autoload
(defun er/mark-sentence ()
  "Marks one sentence."
  (interactive)
  (forward-char 1)
  (backward-sentence 1)
  (set-mark (point))
  (forward-sentence 1)
  (exchange-point-and-mark))

(defun eli/mark-symbol-maybe ()
  (let ((current-length (- (region-end)
						   (region-beginning)))
		(symbol-length (length (thing-at-point 'symbol))))
	(if (/= current-length symbol-length)
		(er/mark-symbol)
	  (setq eli/expand-region-count
			(1+ eli/expand-region-count))
	  (funcall (nth eli/expand-region-count
					eli/expand-region-commands)))))

(defvar eli/expand-region-count 1)
(defvar eli/expand-region-commands '(er/mark-word
									 eli/mark-symbol-maybe
									 er/mark-sentence))

;;;###autoload
(defun eli/expand-region ()
  (interactive)
  (if (eq last-command this-command)
	  (progn
		(cond
		 ((derived-mode-p 'prog-mode)
		  (puni-expand-region))
		 (t (if (< eli/expand-region-count 3)
				(funcall (nth eli/expand-region-count
							  eli/expand-region-commands))
			  (puni-expand-region))))
		(setq eli/expand-region-count
			  (1+ eli/expand-region-count)))
	(if (thing-at-point 'word)
		(er/mark-word)
	  (puni-expand-region))
	(setq eli/expand-region-count 1)))

;;;###autoload
(defun eli/puni-hungry-backward-delete-char ()
  "Hungry delete char backward while keeping expressions balanced.

See `puni-backward-delete-char' for more information."
  (interactive)
  (if (memq (char-before) '(?\t ?\n ?\ ))
      (hungry-delete-backward 1)
    (puni-backward-delete-char)))


;;;; provide
(provide 'lib-puni)
;;; lib-puni.el ends here.
