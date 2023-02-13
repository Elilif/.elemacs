;; core-lib.el --- Initialize core lib configurations.	-*- lexical-binding: t; -*-

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

(defun suppress-messages (func &rest args)
  (cl-letf (((symbol-function 'message)
             (lambda (&rest _args) nil)))
    (apply func args)))

;;;###autoload
(defun eli/open-init-file()
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(keymap-global-set "<f5>" 'eli/open-init-file)

;;;###autoload
(defun my-search-with-chrome ()
  "search with chrome."
  (interactive)
  (let ((target (read-string "Search for: ")))
    (browse-url (concat "http://www.google.com/search?q="
			            (url-hexify-string target)))))

;; insert key-sequence

;;;###autoload
(defun eli/insert-key-sequence ()
  (interactive)
  (insert (key-description
           (read-key-sequence-vector "Press a keystrokes:"))))


(defun elemacs-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Calls `completing-read' but returns the value from COLLECTION.

Simple wrapper around the `completing-read' function that assumes
the collection is either an alist, or a hash-table, and returns
the _value_ of the choice, not the selected choice. "
  (cl-flet ((assoc-list-p (obj) (and (listp obj) (consp (car obj)))))
    (let* ((choice
            (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method))
           (results (cond
                     ((hash-table-p collection) (gethash choice collection))
                     ((assoc-list-p collection) (alist-get choice collection def nil 'equal))
                     (t                         choice))))
      (if (listp results) (car results) results))))


(defun auto-recompile-file-maybe ()
  (when (and (fboundp 'vc-root-dir)
			 (string= (vc-root-dir) user-emacs-directory))
	(byte-compile-file buffer-file-name)))

(defun add-after-save-hook ()
  (add-hook 'after-save-hook 'auto-recompile-file-maybe))

(provide 'core-lib)
;;; core-lib.el ends here.