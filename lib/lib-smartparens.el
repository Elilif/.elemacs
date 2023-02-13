;; lib-smartparens.el --- Initialize lib-smartparens configurations.	-*- lexical-binding: t; -*-

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

(defun sp-in-algorithm-p (_id _action _context)
  "Return t if point is inside code, nil otherwise."
  (when (functionp 'xenops-math-parse-algorithm-at-point)
    (xenops-math-parse-algorithm-at-point)))


(defun fix-show-paren-function (fn)
  (cond ((looking-at-p "\\s(") (funcall fn))
	    (t (save-excursion
	         (ignore-errors (backward-up-list))
	         (funcall fn)))))


;;;; provide
(provide 'lib-smartparens)
;;; lib-smartparens.el ends here.
