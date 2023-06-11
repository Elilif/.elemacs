;; lib-ox.el --- Initialize lib-ox configurations.	-*- lexical-binding: t; -*-

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

;; use with #+HTML_HEAD: <link rel="stylesheet"
;; href="https://latex.now.sh/style.css">
(defun eli/org-export-get-special-block-ordinal (orig element info
                                                      &optional types predicate)
  (if (not (eq (org-element-type element) 'special-block))
      (funcall orig element info types predicate)
    (let* ((counter 0)
           (indicator nil)
           (element-type (org-element-property :type element))
           (element-name (org-element-property :name element)))
      (last (org-element-map (plist-get info :parse-tree)
	            (or types (org-element-type element))
	          (lambda (el)
	            (let ((el-type (org-element-property :type el))
                      (el-name (org-element-property :name el)))
                  (cond
                   ((and (string= element-type el-type)
                         (string= element-name el-name))
                    (setq indicator t)
                    (cl-incf counter))
                   ((and (not indicator)
                         (or (string= element-type el-type)
                             (and (member element-type '("theorem" "lemma"))
                                  (member el-type '("theorem" "lemma"))))
                         (not (string= element-name el-name)))
                    (cl-incf counter)))))
	          info)))))

(defun eli/org-html-special-block-filter (orig special-block contents info)
  (let ((new-contents (replace-regexp-in-string
                       "<p>\n\\(\\(?:.*\n\\)*?\\)</p>\\(\\(?:.*\n?\\)*$\\)"
                       "\\1\\2"
                       contents)))
    (funcall orig special-block new-contents info)))

(defun org-pandoc-link (link contents info)
  "Transcode a LINK object.

The registered formatter for the \='pandoc backend is used. If none
exists, transcode using the registered formatter for the \='org
export backend. For fuzzy (internal) links, resolve the link
destination in order to determine the appropriate reference
number of the target Table/Figure/Equation etc. CONTENTS is the
description of the link, as a string, or nil. INFO is a plist
holding contextual information."
  (let ((type (org-element-property :type link)))
    (cond
     ;; Try exporting with a registered formatter for 'pandoc
     ((org-export-custom-protocol-maybe link contents 'pandoc))
     ;; Try exporting with a registered formatter for 'org
     ((org-export-custom-protocol-maybe link contents 'org))

     ;; Otherwise, override fuzzy (internal) links that point to
     ;; numbered items such as Tables, Figures, Sections, etc.
     ((string= type "fuzzy")
	  (let* ((path (org-element-property :path link))
             (destination (org-export-resolve-fuzzy-link link info))
             (dest-type (when destination (org-element-type destination)))
             (number nil))
        ;; Different link targets require different predicates to the
        ;; `org-export-get-ordinal' function in order to resolve to
        ;; the correct number. NOTE: Should be the same predicate
        ;; function as used to generate the number in the
        ;; caption/label/listing etc.
        (cond
         ((eq dest-type 'paragraph)   ; possible figure
          (setq number (org-export-get-ordinal
                        destination info nil #'org-html-standalone-image-p)))

         ((eq dest-type 'latex-environment)
          (setq number (org-export-get-ordinal
                        destination info nil
                        #'org-pandoc--numbered-equation-p)))

         ((eq dest-type 'has-caption) ;; captioned items
          (setq number (org-export-get-ordinal
                        destination info nil #'org-pandoc--has-caption-p))
	      ))

        ;; Numbered items have the number listed in the link
        ;; description, , fall back on the text in `contents'
        ;; if there's no resolvable destination
        (cond
         ;; Numbered items have the number listed in the link description
         (number
          (format "[[#%s][%s]]" path
                  (if (atom number) (number-to-string number)
                    (mapconcat #'number-to-string number ".")))
	      )

         ;; Unnumbered headlines have the heading name in the link
         ;; description
         ((eq dest-type 'headline)
          (format "[[#%s][%s]]" path
                  (org-export-data
                   (org-element-property :title destination) info)))

         ;; No resolvable destination, fallback on the text in `contents'
         ((eq destination nil)
          (when (org-string-nw-p contents) contents))

         ;; Valid destination, but without a numbered caption/equation
         ;; and not a heading, fallback to standard org-mode link format
         (t
          (org-element-link-interpreter link contents)))))

     ;; Otherwise, fallback to standard org-mode link format
     ((org-element-link-interpreter link contents)))))

(defun eli/strip-ws-maybe (text _backend _info)
  (let* ((text (replace-regexp-in-string
                "\\(\\cc\\) *\n *\\(\\cc\\)"
                "\\1\\2" text))) ;; remove whitespace from line break
    text))


;;;; provide
(provide 'lib-ox)
;;; lib-ox.el ends here.
