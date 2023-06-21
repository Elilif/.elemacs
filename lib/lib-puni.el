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
(defun er/mark-symbol ()
  "Mark the entire symbol around or in front of point."
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw"))
    (when (or (looking-at symbol-regexp)
              (looking-back symbol-regexp))
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

;;;###autoload
(defun eli/expand-region ()
  (interactive)
  (cond
   ((derived-mode-p 'prog-mode)
	(puni-expand-region))
   (t
	(if (eq last-command this-command)
		(er/mark-sentence)
	  (er/mark-symbol)))))


;;;; provide
(provide 'lib-puni)
;;; lib-puni.el ends here.
