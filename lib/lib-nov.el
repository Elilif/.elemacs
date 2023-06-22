;; lib-nov.el --- Initialize lib-nov configurations.	-*- lexical-binding: t; -*-

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

;; WORKAROUND: errors while opening `nov' files with Unicode characters
;; @see https://github.com/wasamasa/nov.el/issues/63
(defun my-nov-content-unique-identifier (content)
  "Return the the unique identifier for CONTENT."
  (let* ((name (nov-content-unique-identifier-name content))
         (selector (format "package>metadata>identifier[id='%s']"
                           (regexp-quote name)))
         (id (car (esxml-node-children (esxml-query selector content)))))
    (and id (intern id))))


;;;###autoload
(defun nov-search (pattern)
  (interactive "sEnter search pattern: ")
  (let ((version nov-epub-version)
        (index 1)
        results)
    (while (< index (1- (length nov-documents)))
      (seq-let (id &rest path) (aref nov-documents index)
        (let (;; HACK: this should be looked up in the manifest
              (imagep (seq-find (lambda (item) (string-match-p (car item) path))
                                image-type-file-name-regexps))
              ;; NOTE: allows resolving image references correctly
              (default-directory (file-name-directory path)))
          (unless imagep
            (with-temp-buffer
              (if (and (version< version "3.0") (eq id nov-toc-id))
                  (insert (nov-ncx-to-html path))
                (insert (nov-slurp path)))
              (goto-char (point-min))
              (when (search-forward pattern nil t)
                (nov-render-html)
                (goto-char (point-min))
                (while (search-forward pattern nil t)
                  (push (list (format "%d %s" index
                                      (replace-regexp-in-string "\n" " "
                                                                (thing-at-point 'line)))
                              index (point))
                        results)))))
          (setq index (1+ index)))))
    (seq-let (index point) (alist-get (completing-read "Jump to: " (reverse results)) results
                                      nil nil #'string=)
      (nov-goto-document index)
      (goto-char point))))

;;;; provide
(provide 'lib-nov)
;;; lib-nov.el ends here.
