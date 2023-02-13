;; lib-outli.el --- Initialize lib-outli configurations.	-*- lexical-binding: t; -*-

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

;;;###autoload
(defun elemacs/outline-comment-subtree ()
  "Comment or uncomment the current subtree in an outlined document.
This puts point at the start of the current subtre."
  (interactive)
  (let (beg end)
    (if (outline-on-heading-p)
		;; we are already looking at a heading
		(beginning-of-line)
      ;; else go back to previous heading
      (outline-previous-visible-heading 1))
	(save-excursion
	  (forward-line 1)
	  (setq beg (point))
	  (outline-end-of-subtree)
	  (setq end (point))
	  (comment-or-uncomment-region beg end))))


;;;; provide
(provide 'lib-outli)
;;; lib-outli.el ends here.
