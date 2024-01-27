;; lib-org-simple-ref.el --- Initialize lib-org-simple-ref configurations.  -*- lexical-binding: t; -*-

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
;; Code in this file is mainly taken from `org-ref'
;;
;;

;;; Code:
(require 'consult)
(require 'embark)

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
   (concat "^\\s-*#\\+name:\\s-+" org-ref-label-re)
   ;; labels in latex
   (concat "\\\\label{" org-ref-label-re "}")
   ;; A target, code copied from org-target-regexp and group 1 numbered.
   (let ((border "[^<>\n\r \t]"))
     (format "<<<\\(?1:%s\\|%s[^<>\n\r]*%s\\)>>>"
             border border border))
   ;; A label link
   (concat "label:" org-ref-label-re "\\_>")
   ;; code ref
   "[ 	]*(\\(?2:ref\\):\\(?1:[-a-zA-Z0-9_][-a-zA-Z0-9_ ]*\\))[ 	]*$"
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

(defvar org-simple-ref-narrow '((?c . "coderef")
                                (?s . "src-block")
                                (?i . "ID")
                                (?r . "radio-target")
                                (?h . "cureent-hd"))
  "Used by `consult--read' in `org-simple-ref-read'.

See `consult--read' for details.")

(defun eli/org-ref-label-annotation (candidate)
  "Used by `completing-read'."
  (let ((plist (alist-get candidate org-ref-label-annot-cache nil nil #'string=)))
    (concat (truncate-string-to-width (propertize (plist-get plist :title)
                                                  'face 'mindre-keyword)
                                      70 nil 32)
            (truncate-string-to-width (propertize (plist-get plist :type)
                                                  'face 'mindre-faded)
                                      70 nil 32))))

;;;###autoload
(defun eli/org-ref-insert-ref-link ()
  "Completion function for a ref link."
  (interactive)
  (let* ((label-trim)
         (plist-getter (lambda ()
                         (let* ((label (org-simple-ref-read (org-ref-get-labels)))
                                ;; or use `completing-read':
                                ;; (completion-extra-properties '(:annotation-function
                                ;;                                eli/org-ref-label-annotation))
                                ;; (label (completing-read (org-ref-get-labels)))
                                )
                           (setq label-trim (string-trim label))
                           (alist-get label org-ref-label-cache nil nil #'string=))))
         (plist (cond
                 ((org-src-edit-buffer-p)
                  (org-src-do-at-code-block
                   (funcall plist-getter)))
                 (t (funcall plist-getter))))
         (result  (pcase (plist-get plist :type)
                    ("ID"
                     (cons (concat "id:" label-trim)
                           (plist-get plist :title)))
                    ("CUSTOM_ID"
                     (cons (concat "#" label-trim)
                           (plist-get plist :title)))
                    ("coderef"
                     (cons (format "(%s)" label-trim)
                           label-trim))
                    (_
                     (cons label-trim
                           label-trim))))
         (link (cond
                ((string= (plist-get plist :type) "radio-target")
                 label-trim)
                ((or (org-src-edit-buffer-p) (get-text-property (point) 'src-block))
                 (format "<<%s>>" (car result)))
                (t
                 (format "[[%s][%s]]" (car result) (cdr result))))))
    (insert link)))

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
       (/= (buffer-chars-modified-tick)
           org-ref-buffer-chars-modified-tick))
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
                     id (propertize (match-string-no-properties 1)
                                    'org-simple-ref--location
                                    (match-beginning 1))
                     data (list
                           :title (if (equal (car oe) 'latex-environment)
                                      ""
                                    (or (and (not (string= (org-element-property :name oe) id))
                                             (org-element-property :name oe))
                                        (org-element-property :raw-value (org-element-lineage oe '(headline)))
                                        (file-name-base (buffer-file-name))))
                           :type (or (and (string= (match-string-no-properties 2) "ref") "coderef")
                                     (org-element-property :key oe)
                                     (symbol-name (car oe))))))
             (cl-pushnew (cons id data) labels))))

        ;; reverse so they are in the order we find them.
        (setq
         org-ref-buffer-chars-modified-tick (buffer-chars-modified-tick)
         org-ref-label-cache (delete-dups (reverse labels)))))
  ;; retrieve the cached data
  (setq org-ref-label-annot-cache org-ref-label-cache))

(defun org-simple-ref-read (collection)
  "Select an item from COLLECTION returned by `org-ref-get-labels'."

  ;; add some metadata (text properties) to candidates.
  (mapc
   (lambda (cand)
     (let* ((string (car cand))
            (data (cdr cand)))
       (setf (car cand)
             (propertize
              (truncate-string-to-width string 70 nil 32)
              'org-simple-ref--type
              (or (car-safe (rassoc (plist-get data :type) org-simple-ref-narrow))
                  ;; 0 is a placeholder
                  0)))))
   collection)
  (let* ((hd (save-excursion
               (org-back-to-heading-or-point-min t)
               (org-element-at-point)))
         (beg (org-element-property :begin hd))
         (end (org-element-property :end hd)))
    (consult--read collection
                   :prompt "Select: "
                   :group
                   (lambda (cand transform)
                     (let* ((plist (alist-get cand collection nil nil #'string=))
                            (type (plist-get plist :type)))
                       (if transform cand type)))
                   :annotate
                   (lambda (cand)
                     (let* ((plist (alist-get cand collection nil nil #'string=))
                            (title (plist-get plist :title)))
                       title))
                   :category 'org-simple-ref
                   :narrow
                   org-simple-ref-narrow
                   :predicate
                   (lambda (cand)
                     (let* ((cand (car cand))
                            (key (get-text-property 0 'org-simple-ref--type cand))
                            (pos (get-text-property 0 'org-simple-ref--location cand)))
                       (if consult--narrow
                           (cond
                            ((= consult--narrow ?h)
                             (and (> pos beg)
                                  (< pos end)))
                            (t
                             (= key consult--narrow)))
                         (/= key ?g)))))))

(defun embark-org-simple-ref-goto-location (target)
  "Jump to org simple reference location TARGET."
  (consult--jump (get-text-property 0 'org-simple-ref--location target))
  (pulse-momentary-highlight-one-line (point)))

(defvar-keymap embark-org-simple-ref-map
  :doc "Keymap for actions on org simple reference."
  :parent embark-general-map
  "g" #'embark-org-simple-ref-goto-location)

(add-to-list 'embark-keymap-alist '(org-simple-ref . embark-org-simple-ref-map))


;;;; provide
(provide 'lib-org-simple-ref)
;;; lib-org-simple-ref.el ends here.
