;; lib-pdf.el --- Initialize lib-pdf configurations.	-*- lexical-binding: t; -*-

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
(defun +pdf-keyboard-select-region (&optional all-pages-p)
  "Ref: https://github.com/dalanicolai/dala-emacs-lisp/blob\
/9662aa2ab993157e6e7587385d27c48ed50a1a50/pdf-keyboard-highlight.el#L79"
  (interactive "P")
  (pdf-view-deactivate-region)
  (let* ((pages (if all-pages-p nil (pdf-view-current-page)))
	     (candidates (mapcar (lambda (x)
			                   (list (cdar (cdr x)) (cdar x) (cdar (cddr x))))
			                 (pdf-info-search-regexp
                              (read-string "Regexp: ") pages)))
         (page-edges-list (alist-get (completing-read
                                      "Select correct context: " candidates)
                                     candidates nil nil 'equal))
	     (edges-list (cadr page-edges-list))
         (edges (append (cl-subseq
                         (car edges-list) 0 2)
                        (cl-subseq (car (last edges-list)) 2 4))))
    (pdf-view-goto-page (car page-edges-list))
    (setq pdf-view-active-region (list edges))
    (pdf-view--push-mark)
    (pdf-view-display-region)))


;;;; provide
(provide 'lib-pdf)
;;; lib-pdf.el ends here.
