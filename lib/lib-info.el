;; lib-info.el --- Initialize lib-info configurations.	-*- lexical-binding: t; -*-

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

;; copy info url
(defvar eli/Info-url-alist
  '(("org" . "https://orgmode.org/manual/")
    ("emacs" . "https://www.gnu.org/software/emacs/manual/html_node/emacs/")
    ("elisp" . "https://www.gnu.org/software/emacs/manual/html_node/elisp/"))
  "Official manual URL for `eli/Info-url-for-node'.")

(defun eli/Info-url-for-node (node)
  "Return a URL for NODE.

NODE should be a string on the form \"(manual)Node\"."
  (unless (string-match "\\`(\\(.+\\))\\(.+\\)\\'" node)
    (error "Invalid node name %s" node))
  (let ((manual (match-string 1 node))
        (node (match-string 2 node)))
    ;; Encode a bunch of characters the way that makeinfo does.
    (setq node
          (mapconcat (lambda (ch)
                       (if (or (< ch 32)        ; ^@^A-^Z^[^\^]^^^-
                               (<= 33 ch 47)    ; !"#$%&'()*+,-./
                               (<= 58 ch 64)    ; :;<=>?@
                               (<= 91 ch 96)    ; [\]_`
                               (<= 123 ch 127)) ; {|}~ DEL
                           (format "_00%x" ch)
                         (char-to-string ch)))
                     node
                     ""))
    (concat (cdr (assoc manual eli/Info-url-alist #'equal))
            (url-hexify-string (string-replace " " "-" node))
            ".html")))

;;;###autoload
(defun eli/Info-copy-node-url (node)
  "Put the online url of the current Info NODE into the kill ring.

By default, go to the current Info node."
  (interactive (list (Info-read-node-name
                      "Go to node (default current page): " Info-current-node))
               Info-mode)
  (kill-new
   (eli/Info-url-for-node (format "(%s)%s" (file-name-sans-extension
                                            (file-name-nondirectory
                                             Info-current-file))
                                  node))))


;;;; provide
(provide 'lib-info)
;;; lib-info.el ends here.
