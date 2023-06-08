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


;;;; provide
(provide 'lib-nov)
;;; lib-nov.el ends here.
