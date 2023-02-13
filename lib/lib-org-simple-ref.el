;; lib-org-simple-ref.el --- Initialize lib-org-simple-ref configurations.	-*- lexical-binding: t; -*-

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

;;; cross-reference in org-mode
(defvar org-ref-label-re
  (rx-to-string
   '(group-n 1 (one-or-more (any word "-.:?!`'/*@+|(){}<>&_^$#%~"))))
  "Regexp for labels.")

(defvar org-ref-ref-label-regexps
  (list
   (concat ":ID:\\s-+" org-ref-label-re "\\_>")
   ;; CUSTOM_ID in a heading
   (concat ":CUSTOM_ID:\\s-+" org-ref-label-re "\\_>")
   ;; #+name
   (concat "^\\s-*#\\+name:\\s-+" org-ref-label-re "\\_>")
   ;; labels in latex
   (concat "\\\\label{" org-ref-label-re "}")
   ;; A target, code copied from org-target-regexp and group 1 numbered.
   (let ((border "[^<>\n\r \t]"))
     (format "<<\\(?1:%s\\|%s[^<>\n\r]*%s\\)>>"
	         border border border))
   ;; A label link
   (concat "label:" org-ref-label-re "\\_>")
   "\\\\lstset{.*label=\\(?1:.*?\\),.*}")
  "List of regular expressions to labels.
The label should always be in group 1.")

(defvar-local org-ref-label-cache nil
  "Buffer-local cache variable for labels.")
(defvar-local org-ref-buffer-chars-modified-tick nil
  "Buffer-local variable to hold `buffer-chars-modified-tick'.")
(defvar org-ref-label-annot-cache nil
  "Store the value of `org-ref-label-cache'.

 used by `eli/org-ref-label-annotation'.")

(defun eli/org-ref-label-annotation (candidate)
  (let ((plist (alist-get candidate org-ref-label-annot-cache nil nil #'string=)))
    (concat (truncate-string-to-width (propertize (plist-get plist :title)
                                                  'face 'mindre-keyword)
                                      70 nil 32)
            (truncate-string-to-width (propertize (plist-get plist :type)
                                                  'face 'mindre-faded)
                                      70 nil 32))))

(defun eli/org-ref-insert-ref-link ()
  "Completion function for a ref link."
  (interactive)
  (let* ((completion-extra-properties '(:annotation-function
                                        eli/org-ref-label-annotation))
         (label (completing-read "Choose: " (org-ref-get-labels)))
         (label-trim (string-trim label))
         (plist (alist-get label org-ref-label-cache nil nil #'string=))
         (result  (pcase (plist-get plist :type)
                    ("ID"
                     (cons (concat "id:" label-trim)
                           (plist-get plist :title)))
                    ("CUSTOM_ID"
                     (cons (concat "#" label-trim)
                           (plist-get plist :title)))
                    ("latex-environment"
                     (cons label-trim
                           label-trim)))))
    (insert (format "[[%s][%s]]" (car result) (cdr result)))))

(defun org-ref-get-labels ()
  "Return a list of referenceable labels in the document.
You can reference:
A NAME keyword
A CUSTOM_ID property on a heading
A LaTeX label
A target.
A label link
A setting in lstset

See `org-ref-ref-label-regexps' for the patterns that find these.

Returns a list of cons cells (label . context).

It is important for this function to be fast, since we use it in
font-lock."
  (if (or
       ;; if we have not checked we have to check
       (null org-ref-buffer-chars-modified-tick)
       ;; Now check if buffer has changed since last time we looked. We check
       ;; this with the buffer-chars-modified-tick which keeps track of changes.
       ;; If this hasn't changed, no chars have been modified.
       (not (= (buffer-chars-modified-tick)
	           org-ref-buffer-chars-modified-tick)))
      ;; We need to search for all the labels either because we don't have them,
      ;; or the buffer has changed since we looked last time.
      (let ((case-fold-search t)
	        (rx (string-join org-ref-ref-label-regexps "\\|"))
	        (labels '())
	        oe ;; org-element
	        data
            id)
	    (save-excursion
	      (org-with-wide-buffer
	       (goto-char (point-min))
	       (while (re-search-forward rx nil t)
	         (save-match-data
	           (setq oe (org-element-context)
                     id (match-string-no-properties 1)
                     data (list
                           :title (if (equal (car oe) 'latex-environment)
                                      ""
                                    (or (org-element-property :raw-value (org-element-lineage oe '(headline)))
                                        (file-name-base (buffer-file-name))))
                           :type (or (org-element-property :key oe)
                                     (symbol-name (car oe))))))
	         (cl-pushnew (cons (truncate-string-to-width id 70 nil 32) data) labels))))

	    ;; reverse so they are in the order we find them.
	    (setq
	     org-ref-buffer-chars-modified-tick (buffer-chars-modified-tick)
	     org-ref-label-cache (delete-dups (reverse labels)))))
  ;; retrieve the cached data
  (setq org-ref-label-annot-cache org-ref-label-cache))


;;;; provide
(provide 'lib-org-simple-ref)
;;; lib-org-simple-ref.el ends here.
