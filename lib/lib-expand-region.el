;; lib-expand-region.el --- Initialize lib-expand-region configurations.	-*- lexical-binding: t; -*-

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

;;;; expand-region lib
(with-eval-after-load 'org
  (defmacro org-fold-core-save-visibility (use-markers &rest body)
    "Save and restore folding state around BODY.
If USE-MARKERS is non-nil, use markers for the positions.  This
means that the buffer may change while running BODY, but it also
means that the buffer should stay alive during the operation,
because otherwise all these markers will point to nowhere."
    (declare (debug (form body)) (indent 1))
    (org-with-gensyms (regions)
	  `(let* ((,regions ,(org-fold-core-get-regions :with-markers use-markers)))
		 (unwind-protect (progn ,@body)
		   (org-fold-core-regions ,regions :override nil :clean-markers nil))))))

(with-eval-after-load 'the-org-mode-expansions
  (defun er/save-org-mode-excursion (action)
    "Save outline visibility while expanding in org-mode"
    (org-fold-core-save-visibility t
	  (funcall action))))

;;;###autoload
(defun er/mark-block-comment ()
  "Mark the entire comment around point."
  (interactive)
  (when (er--point-is-in-comment-p)
    (let ((p (point)))
      (while (and (er--point-is-in-comment-p) (not (eobp)))
        (forward-char 1)
        (skip-chars-forward "\n\r "))
      (beginning-of-line)
      (backward-char 1)
      (set-mark (point))
      (goto-char p)
      (while (er--point-is-in-comment-p)
        (backward-char 1)
        (skip-chars-backward "\n\r "))
      (forward-char 1))))

(defun eli/er-clearn-history (_arg)
  (if (not (memq last-command '(er/expand-region er/contract-region)))
      (er/clear-history)))


;;;; provide
(provide 'lib-expand-region)
;;; lib-expand-region.el ends here.
