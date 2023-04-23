;; lib-mu4e.el --- Initialize lib-mu4e configurations.	-*- lexical-binding: t; -*-

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


;;;; provide
(provide 'lib-mu4e)
;;; lib-mu4e.el ends here.
