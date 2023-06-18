;; lib-org-roam.el --- Initialize lib-org-roam configurations.	-*- lexical-binding: t; -*-

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

(cl-eval-when (compile)
  (require 'org-roam))

;; Codes blow are used to general a hierachy
;; for title nodes that under a file
(cl-defmethod org-roam-node-doom-filetitle ((node org-roam-node))
  "Return the value of \"#+title:\" (if any) from file that NODE resides in.
      If there's no file-level title in the file, return empty string."
  (or (if (= (org-roam-node-level node) 0)
          (org-roam-node-title node)
        (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
	  ""))
(cl-defmethod org-roam-node-doom-hierarchy ((node org-roam-node))
  "Return hierarchy for NODE, constructed of its file title, OLP and
direct title.
        If some elements are missing, they will be stripped out."
  (let ((title     (org-roam-node-title node))
        (olp       (org-roam-node-olp   node))
        (level     (org-roam-node-level node))
        (filetitle (org-roam-node-doom-filetitle node))
        (separator (propertize " > " 'face 'shadow)))
	(cl-case level
	  ;; node is a top-level file
	  (0 filetitle)
	  ;; node is a level 1 heading
	  (1 (concat (propertize filetitle 'face '(shadow italic))
                 separator title))
	  ;; node is a heading with an arbitrary outline path
	  (t (concat (propertize filetitle 'face '(shadow italic))
                 separator (propertize (string-join olp " > ")
                                       'face '(shadow italic))
                 separator title)))))

(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
	  (file-name-nondirectory
	   (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
	(error "")))

(cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
  "Access slot \"backlinks\" of org-roam-node struct CL-X"
  (let* ((count (caar (org-roam-db-query
			           [:select (funcall count source)
				                :from links
				                :where (= dest $s1)
				                :and (= type "id")]
			           (org-roam-node-id node)))))
	(format "[%d]" count)))

(cl-defmethod org-roam-node-backlinkscount-number ((node org-roam-node))
  "Access slot \"backlinks\" of org-roam-node struct CL-X. This
     is identical to `org-roam-node-backlinkscount' with the
     difference that it returns a number instead of a fromatted
     string. This is to be used in
     `eli/org-roam-node-sort-by-backlinks'"
  (let* ((count (caar (org-roam-db-query
			           [:select (funcall count source)
				                :from links
				                :where (= dest $s1)
				                :and (= type "id")]
			           (org-roam-node-id node)))))
	count))


(defun eli/org-roam-node-sort-by-backlinks (completion-a completion-b)
  "Sorting function for org-roam that sorts the list of nodes by
   the number of backlinks. This is the sorting function in
   `eli/org-roam-backlinks--read-node-backlinks'"
  (let ((node-a (cdr completion-a))
	    (node-b (cdr completion-b)))
	(>= (org-roam-node-backlinkscount-number node-a)
	    (org-roam-node-backlinkscount-number node-b))))

;; embark support
;; from https://github.com/Vidianos-Giannitsis/Dotfiles/blob/master/emacs/
;; .emacs.d/libs/zettelkasten.org
(defun eli/org-roam-backlinks-query* (NODE)
  "Gets the backlinks of NODE with `org-roam-db-query'."
  (org-roam-db-query
   [:select [source dest]
		    :from links
		    :where (= dest $s1)
		    :and (= type "id")]
   (org-roam-node-id NODE)))

(defun eli/org-roam-backlinks-p (SOURCE NODE)
  "Predicate function that checks if NODE is a backlink of SOURCE."
  (let* ((source-id (org-roam-node-id SOURCE))
	     (backlinks (eli/org-roam-backlinks-query* SOURCE))
	     (id (org-roam-node-id NODE))
	     (id-list (list id source-id)))
    (member id-list backlinks)))

(defun eli/org-roam-backlinks--read-node-backlinks (source)
  "Runs `org-roam-node-read' on the backlinks of SOURCE.
 The predicate used as `org-roam-node-read''s filter-fn is
 `eli/org-roam-backlinks-p'."
  (org-roam-node-read nil (apply-partially #'eli/org-roam-backlinks-p source)
                      #'eli/org-roam-node-sort-by-backlinks))

(defun eli/org-roam-backlinks-node-read (entry)
  "Read a NODE and run `eli/org-roam-backlinks--read-node-backlinks'."
  (let* ((node (get-text-property 0 'node entry))
         (backlink (eli/org-roam-backlinks--read-node-backlinks node)))
    (find-file (org-roam-node-file backlink))))


(defun consult-org-headline-insert-backlink (target)
  (let* ((marker (plist-get
                  (text-properties-at 0 target)
                  'consult--candidate))
         (headline-name (substring (org-no-properties target)
                                   0 -1))
         (headline-id (save-excursion
                        (with-current-buffer
                            (marker-buffer marker)
                          (goto-char marker)
                          (org-id-get-create)))))
    (org-insert-link
	 nil (concat "id:" headline-id) headline-name)))

(defun consult-org-headline-insert-reference (target)
  (let* ((headline (substring (org-no-properties target)
                              0 -1))
         (headline-name (car
                         (last
                          (split-string
                           (replace-regexp-in-string "\\*+ " "" headline)
                           "/")))))
    (insert (format "[[%s]]" headline-name))))

(defun eli/update-org-roam-db ()
  (while-no-input
	(dolist (buf (org-roam-buffer-list))
	  (when (buffer-modified-p buf)
		(with-current-buffer buf
		  (let ((file (buffer-file-name buf)))
			(org-roam-headline-db--update-file file)
			(org-roam-db-update-file file)))))))

(defun eli/org-roam-filter-books (node)
  (let ((file (org-roam-node-file node)))
	(not (string-match-p "books/" file))))

;;;###autoload
(defun eli/org-roam-node-find (&optional other-window)
  (interactive current-prefix-arg)
  (org-roam-node-find other-window nil #'eli/org-roam-filter-books))

;;;; provide
(provide 'lib-org-roam)
;;; lib-org-roam.el ends here.
