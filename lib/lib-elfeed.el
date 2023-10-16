;; lib-elfeed.el --- Initialize lib-elfeed configurations.  -*- lexical-binding: t; -*-

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
    (let ((elfeed-search-sort-function nil))
      (elfeed-search-set-filter
       (concat
        "="
        (replace-regexp-in-string
         (rx "?" (* not-newline) eos)
         ""
         (elfeed-feed-url (elfeed-entry-feed entry))))))))


;;;###autoload
(defun eli/elfeed-search-starred-entries ()
  (interactive)
  (elfeed-search-set-filter "+starred"))

(defface elfeed-search-starred-title-face
  '((t :foreground "#f77"))
  "Marks a starred Elfeed entry."
  :group 'elfeed)

(defalias 'elfeed-toggle-star
  (elfeed-expose #'elfeed-search-toggle-all 'starred))

;;;###autoload
(defun eli/filter-read ()
  (interactive)
  (elfeed-search-set-filter "@7-days-ago -unread +A"))

;;;###autoload
(defun eli/elfeed-overview ()
  "Get an overview of all feeds."
  (interactive)
  (with-current-buffer (elfeed-search-buffer)
    (elfeed-save-excursion
      (let* ((inhibit-read-only t)
             (standard-output (current-buffer)))
        (erase-buffer)
        (eli/elfeed-overview--update-list)
        (dolist (entry elfeed-search-entries)
          (funcall elfeed-search-print-entry-function entry)
          (insert "\n"))
        (setf elfeed-search-last-update (float-time))))
    (when (zerop (buffer-size))
      ;; If nothing changed, force a header line update
      (force-mode-line-update))
    (run-hooks 'elfeed-search-update-hook)))

(defun eli/elfeed-overview--update-list ()
  "Update `elfeed-search-filter' list."
  (let* ((head (list nil))
         (tail head)
         (count 0))
    (dolist (feed elfeed-feeds)
      (let* ((lexical-binding t)
             (filter (elfeed-search-parse-filter
                      (concat "=" (or (car-safe feed)
                                      feed))))
             (func (byte-compile (elfeed-search-compile-filter filter))))
        (with-elfeed-db-visit (entry feed)
          (when (funcall func entry feed count)
            (setf (cdr tail) (list entry)
                  tail (cdr tail)
                  count (1+ count))
            (elfeed-db-return)))))
    (let ((entries (cdr head))
          (elfeed-search-sort-function
           (lambda (a b)
             (let ((a-date (elfeed-entry-date a))
                   (b-date (elfeed-entry-date b)))
               (> a-date b-date)))))
      (setf entries (sort entries elfeed-search-sort-function))
      (setf elfeed-search-entries
            entries))))

;;;###autoload
(defun eli/elfeed-search-quit-and-kill-buffers ()
  "Save the database, then kill elfeed buffers, asking the user
for confirmation when needed."
  (interactive)
  (elfeed-db-save)
  (let (buf)
    (dolist (file rmh-elfeed-org-files)
      (when (and (setq buf (get-file-buffer file))
                 (buffer-modified-p buf)
                 (y-or-n-p (format "Save file %s? " file)))
        (with-current-buffer buf (save-buffer))
        (kill-buffer buf))))
  (kill-buffer "*elfeed-log*")
  (kill-buffer (current-buffer)))

(defvar eli/elfeed-filters '("@7-days-ago +A"
                             "@7-days-ago +B +TEP"))

;;;###autoload
(defun eli/elfeed-search-set-filter ()
  "Set a new search filter for the elfeed-search buffer."
  (interactive)
  (elfeed-search-set-filter (completing-read "Filter: " eli/elfeed-filters)))


;; SRC: https://github.com/skeeto/elfeed/issues/392
(defun sk/elfeed-db-remove-entry (id)
  "Removes the entry for ID"
  (avl-tree-delete elfeed-db-index id)
  (remhash id elfeed-db-entries))

(defun sk/elfeed-search-remove-selected ()
  "Remove selected entries from database"
  (interactive)
  (let* ((entries (elfeed-search-selected))
         (count (length entries)))
    (when (y-or-n-p (format "Delete %d entires?" count))
      (cl-loop for entry in entries
               do (sk/elfeed-db-remove-entry (elfeed-entry-id entry)))))
  (elfeed-search-update--force))


;;;; provide
(provide 'lib-elfeed)
;;; lib-elfeed.el ends here.
