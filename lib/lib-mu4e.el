;; lib-mu4e.el --- Initialize lib-mu4e configurations.  -*- lexical-binding: t; -*-

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

;; override original `mu4e--view-activate-urls'
(defun mu4e--view-activate-urls ()
  "Turn things that look like URLs into clickable things.
Also number them so they can be opened using `mu4e-view-go-to-url'."
  (let ((num 0))
    (save-excursion
      (setq mu4e--view-link-map ;; buffer local
            (make-hash-table :size 32 :weakness nil))
      (goto-char (point-min))
      (while (re-search-forward mu4e--view-beginning-of-url-regexp nil t)
        (let ((bounds (thing-at-point-bounds-of-url-at-point)))
          (when bounds
            (let* ((url (thing-at-point-url-at-point))
                   (ov (make-overlay (car bounds) (cdr bounds))))
              (puthash (cl-incf num) url mu4e--view-link-map)
              (add-text-properties
               (car bounds)
               (cdr bounds)
               `(face mu4e-link-face
                      mouse-face highlight
                      mu4e-url ,url
                      keymap ,mu4e-view-active-urls-keymap
                      help-echo
                      "[mouse-1] or [M-RET] to open the link"))
              (overlay-put ov 'invisible t)
              (overlay-put ov 'after-string
                           (propertize (format "\u200B[%d]" num)
                                       'face 'mu4e-url-number-face)))))))))

;; filter
(defun eli/mu4e-search-filter-source ()
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (sender-email (plist-get (car (plist-get msg :from)) :email)))
    (mu4e--search-execute (concat "from:" sender-email) nil)))


;; SRC: https://emacs-china.org/t/topic/305/77?u=vagrantjoker
;; Xapian, the search engine of mu has a poor support of CJK characters,
;; which causes only query contains no more than 2 CJK characters works.
;;
;; https://researchmap.jp/?page_id=457
;;
;; This workaroud breaks any CJK words longer than 2 characters into
;; combines of bi-grams. Example: 我爱你 -> (我爱 爱你)
;;
(defun mu4e-goodies~break-cjk-word (word)
  "Break CJK word into list of bi-grams like: 我爱你 -> 我爱 爱你"
  (if (or (<= (length word) 2)
          (equal (length word) (string-bytes word))) ; only ascii chars
      word
    (let ((pos nil)
          (char-list nil)
          (br-word nil))
      (if (setq pos (string-match ":" word))     ; like: "s:abc"
          (concat (substring word 0 (+ 1 pos))
                  (mu4e-goodies~break-cjk-word (substring word (+ 1 pos))))
        (if (memq 'ascii (find-charset-string word)) ; ascii mixed with others like: abcあいう
            word
          (progn
            (setq char-list (split-string word "" t))
            (while (cdr char-list)
              (setq br-word (concat br-word (concat (car char-list) (cadr char-list)) " "))
              (setq char-list (cdr char-list)))
            br-word))))))

(defun mu4e-goodies~break-cjk-query (expr)
  "Break CJK strings into bi-grams in query."
  (let ((word-list (split-string expr " " t))
        (new ""))
    (dolist (word word-list new)
      (setq new (concat new (mu4e-goodies~break-cjk-word word) " ")))))

;;;; provide
(provide 'lib-mu4e)
;;; lib-mu4e.el ends here.
