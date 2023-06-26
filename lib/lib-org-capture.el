;; lib-org-capture.el --- Initialize lib-org-capture configurations.	-*- lexical-binding: t; -*-

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

(defun eli/capture-report-date-file ()
  (let ((name (read-string "Name: ")))
    (expand-file-name (format "%s-%s.org"
				              (format-time-string "%Y-%m-%d")
				              name) "~/Dropbox/org/blog")))

;; add a property to create id
(defun eli/org-capture-maybe-create-id ()
  (when (org-capture-get :create-id)
    (org-id-get-create)))

;; from: https://stackoverflow.com/questions/21073859/is-there-a-way-
;; with-org-capture-templates-to-not-insert-a-line-if-initial-conten
(defun v-i-or-nothing ()
  (with-current-buffer (org-capture-get :original-buffer)
    (let ((v-i (plist-get org-store-link-plist :initial))
          (org-src-mode (replace-regexp-in-string
			             "-mode"
			             ""
			             (format "%s" major-mode)))
          (type (if (derived-mode-p 'prog-mode) "src" "quote")))
      (if (equal v-i "")
          ""
	    (if (string= type "src")
            (concat (format "\n#+begin_%s %s\n" type org-src-mode)
                    v-i
                    (format "\n#+end_%s\n" type))
          (concat (format "\n#+begin_%s\n" type)
                  v-i
                  (format "\n#+end_%s\n" type)))))))

(defun v-a-or-nothing ()
  (with-current-buffer (org-capture-get :original-buffer)
    (let* ((v-a (plist-get org-store-link-plist :annotation))
           (v-a-empty-p (equal v-a ""))
           (file-name (buffer-file-name (current-buffer))))
      (cond
       (v-a-empty-p "")
       ((and (not v-a-empty-p) (eq major-mode 'org-mode))
        (concat "- reference :: "
                (replace-regexp-in-string "::\\(.*?\\)\\]\\[\\(.*?\\)\\]" "\\2" v-a nil nil 1)))
       ((and (not (string-match-p "::\\(.*?\\)\\]\\[\\(.*?\\)\\]" v-a))
             file-name)
        (concat "- reference :: "
                (substring v-a 0 -1)
                "["
                (file-name-nondirectory file-name)
                "]]"))
       (t
        (concat "- reference :: " v-a))))))

(defun v-i-or-nothing-word ()
  (let* ((v-i (plist-get org-store-link-plist :initial))
		 (sentence (with-current-buffer (org-capture-get :original-buffer)
					 (thing-at-point 'sentence 'no-properties)))
         (new-string (string-clean-whitespace
                      (replace-regexp-in-string "\n" " " (if (string-empty-p v-i)
															 sentence
														   v-i)))))
    new-string))

;; better fill region in capture
(defun eli/fill-quote-and-checklist ()
  "Fill long quotes or checklist after capture them."
  (save-excursion
    (push-mark)
    (goto-char (point-min))
    (cond  
     ((save-excursion
        (re-search-forward "#\\+begin_quote\n\\(\\(?:.*\n\\)*?\\)#\\+end_quote"
                           nil t))
      (fill-region (match-beginning 1)
                   (match-end 1)))
     ((save-excursion
        (re-search-forward "^- \\[ \\].*" nil t))
      (fill-region (match-beginning 0)
                   (match-end 0))))))

(defun eli/org-capture-template-goto-today (format-string start end point)
  "Set point for capturing at what capture target file+headline
with headline set to %l would do."
  (org-capture-put :target (list 'file+headline
                                 (nth 1 (org-capture-get :target))
                                 (format-time-string format-string)))
  (org-capture-put-target-region-and-position)
  (widen)
  (let ((hd (nth 2 (org-capture-get :target))))
    (goto-char (point-min))
    (if (re-search-forward
	     (format org-complex-heading-regexp-format
                 (regexp-quote (substring hd start end)))
	     nil t)
	    (goto-char (line-beginning-position))
	  (goto-char point)
	  (insert "\n")
	  (insert "* " hd "\n")
	  (beginning-of-line 0))))


;;;; provide
(provide 'lib-org-capture)
;;; lib-org-capture.el ends here.
