;; lib-elfeed.el --- Initialize lib-elfeed configurations.	-*- lexical-binding: t; -*-

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

;;;;; elfeed lib
;; Filter elfeed search buffer by the feed under cursor.

;;;###autoload
(defun eli/elfeed-search-filter-source (entry)
  "Filter elfeed search buffer by the feed under cursor."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (elfeed-search-set-filter
     (concat
	  "@6-months-ago "
	  ;; "+unread "
	  "="
	  (replace-regexp-in-string
	   (rx "?" (* not-newline) eos)
	   ""
	   (elfeed-feed-url (elfeed-entry-feed entry)))))))


;;;###autoload
(defun eli/elfeed-search-starred-entries ()
  (interactive)
  (elfeed-search-set-filter "+starred"))

(defface elfeed-search-starred-title-face
  '((t :foreground "#f77"))
  "Marks a starred Elfeed entry."
  :group 'elfeed)

;;;###autoload
(defun eli/filter-read ()
  (interactive)
  (elfeed-search-set-filter "@7-days-ago -unread +A"))

;;;###autoload
(defun eli/elfeed-search-quit-and-kill-buffers ()
  "Save the database, then kill elfeed buffers, asking the user
for confirmation when needed."
  (interactive)
  (elfeed-db-save)
  (let (buf)
    (dolist (file rmh-elfeed-org-files)
	  (setq buf (get-file-buffer file))
	  (when (and (buffer-modified-p buf)
				 file
				 (y-or-n-p (format "Save file %s? " file)))
        (with-current-buffer buf (save-buffer)))
	  (kill-buffer buf)))
  (kill-buffer "*elfeed-log*")
  (kill-buffer (current-buffer)))



;;;; provide
(provide 'lib-elfeed)
;;; lib-elfeed.el ends here.
