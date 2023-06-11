;; org-roam-headline.el --- Initialize org-roam-headline configurations.	-*- lexical-binding: t; -*-

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

(defvar org-roam-headline--db nil)

(defcustom org-roam-headline-db-location "~/.emacs.d/var/org/org-roam-headline.db"
  "Default db locationl"
  :group 'org-roam-headline)

(defcustom org-roam-headline-db-update-on-save t
  "If t, update the Org-roam database upon saving the file.

Disable this if your files are large and updating the database is
slow."
  :group 'org-roam-headline)

(cl-defstruct (org-roam-headline--node (:constructor org-roam-headline--node-create)
									   (:copier nil))
  "A heading or top level file with an assigned ID property."
  id title headline file)

(defun org-roam-headline-db--close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for the database in
the current `org-roam-directory'."
  (unless db
    (setq db org-roam-headline--db))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)))

(defun org-roam-headline-db--init (db)
  "Initialize database DB with the correct schema and user version."
  (emacsql-with-transaction db
    (emacsql db [:create-table headlines ([(id :primary-key)
										   (title :not-null)
										   (headline :not-null)
										   (file :not-null)])])))

(defun org-roam-headline-db ()
  (unless (and org-roam-headline--db
			   (emacsql-live-p org-roam-headline--db))
	(let ((init-db (not (file-exists-p org-roam-headline-db-location))))
	  (make-directory (file-name-directory org-roam-headline-db-location) t)
	  (let ((conn (emacsql-sqlite-builtin org-roam-headline-db-location)))
		(emacsql conn [:pragma (= foreign_keys ON)])
		(when-let* ((process (emacsql-process conn))
                    (_ (processp process)))
          (set-process-query-on-exit-flag process nil))
		(when init-db
          (org-roam-headline-db--init conn))
		(setq org-roam-headline--db conn))))
  org-roam-headline--db)

(defun org-roam-headline-db--query (sql &rest args)
  "Run SQL query on Org-roam database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (apply #'emacsql org-roam-headline--db sql args))

(defun org-roam-headline-db--clear-file (file)
  (org-roam-headline-db--query [:delete :from headlines
										:where (= file $s1)]
							   file))

(defun org-roam-headline-db--insert-entry (file)
  (with-temp-buffer
	(set-buffer-multibyte t)
    (insert-file-contents file)
	(let ((headlines nil)
		  (org-mode-hook nil))
	  (org-mode)
	  (org-map-entries
	   (lambda ()
		 (push (nth 4 (org-heading-components))
			   headlines)))
	  (dolist (headline headlines)
		(let ((id (secure-hash 'sha1 (concat headline file)))
			  (title (org-get-title)))
		  (org-roam-headline-db--query [:insert :into headlines
												:values $v1]
									   (vector id title headline file)))))))

(defun org-roam-headline-db--update-file (&optional file)
  (setq file (or file (buffer-file-name (buffer-base-buffer))))
  (emacsql-with-transaction (org-roam-headline-db)
	(org-roam-headline-db--clear-file file)
	(org-roam-headline-db--insert-entry file)))

;;;###autoload
(defun org-roam-headline-db-sync (&optional force)
  (interactive "P")
  (org-roam-headline-db--close)
  (when force (delete-file org-roam-headline-db-location))
  (org-roam-headline-db)
  (let* ((org-roam-files (org-roam-list-files)))
	(emacsql-with-transaction (org-roam-headline-db)
	  (dolist (file org-roam-files)
		(org-roam-headline-db--clear-file file)
		(org-roam-headline-db--insert-entry file)))))


(defun org-roam-headline-list ()
  "Return all nodes stored in the database as a list of `org-roam-node's."
  (org-roam-headline-db)
  (let ((rows (org-roam-headline-db--query
               [:select [title headline file]
						:from headlines])))
    (cl-loop for row in rows
			 collect (pcase-let* ((`(,title ,headline ,file)
								   row))
					   (org-roam-headline--node-create
						:title title
						:headline headline
						:file file)))))

(defun org-roam-headline-read--completions ()
  (mapcar
   (lambda (node)
	 (let* ((headline (org-roam-headline--node-headline node))
			(name (propertize headline 'node node)))
	   (cons name node)))
   (org-roam-headline-list)))


(defvar-keymap org-roam-headline-map
  "i" #'org-roam-headline-insert)

(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist '(org-roam-headline--node . org-roam-headline-map)))

(defun org-roam-headline-insert (target)
  (let* ((node (plist-get
				(text-properties-at 0 target)
				'node))
		 (file (org-roam-headline--node-file node))
         (headline-name (org-roam-headline--node-headline node))
		 (buffer (find-file-noselect file))
         (headline-id (save-excursion
                        (with-current-buffer buffer
						  (let (pos)
							(org-map-entries
							 (lambda ()
							   (when (string= (nth 4 (org-heading-components)) headline-name)
								 (setq pos (point)))))
							(goto-char pos))
                          (org-id-get-create)))))
    (org-insert-link
	 nil (concat "id:" headline-id) headline-name)))

;;;###autoload
(defun org-roam-headline-read ()
  (interactive)
  (let* ((nodes (org-roam-headline-read--completions))
		 (node (completing-read
				"Select: "
				(lambda (string pred action)
				  (if (eq action 'metadata)
					  '(metadata
						(category . org-roam-headline--node))
					(complete-with-action action nodes string pred))))))
	(cdr (assoc node nodes))))

;;;###autoload
(defun org-roam-headline-node-find (&optional other-window)
  (interactive current-prefix-arg)
  (let ((node (org-roam-headline-read)))
	(org-roam-headline-node-visit node (if other-window
										   #'switch-to-buffer-other-window
										 #'pop-to-buffer-same-window))))

(defun org-roam-headline-node-visit (node &optional cmd)
  (let* ((headline (org-roam-headline--node-headline node))
		 (file (org-roam-headline--node-file node))
		 (buffer (find-file-noselect file))
		 pos)
	(funcall cmd buffer)
	(org-map-entries
	 (lambda ()
	   (when (string= (nth 4 (org-heading-components)) headline)
		 (setq pos (point)))))
	(goto-char pos)))

(define-minor-mode org-roam-headline-autosync-mode
  ""
  :group 'org-roam-headline
  :global t
  :init-value nil
  (cond
   (org-roam-headline-autosync-mode
	(add-hook 'kill-emacs-hook #'org-roam-headline-db--close)
	(add-hook 'find-file-hook  #'org-roam-headline-db-autosync--setup-update-on-save-h)
	(advice-add #'rename-file :after  #'org-roam-headline-db-autosync--rename-file-a)
    (advice-add #'delete-file :before #'org-roam-headline-db-autosync--delete-file-a)
	(org-roam-headline-db-sync))
   (t
	(remove-hook 'kill-emacs-hook #'org-roam-headline-db--close)
	(remove-hook 'find-file-hook  #'org-roam-headline-db-autosync--setup-update-on-save-h)
	(advice-remove #'rename-file #'org-roam-headline-db-autosync--rename-file-a)
    (advice-remove #'delete-file #'org-roam-headline-db-autosync--delete-file-a)
	(org-roam-headline-db--close)
	(dolist (buf (org-roam-buffer-list))
      (with-current-buffer buf
        (remove-hook 'after-save-hook #'org-roam-headline-db--try-update-on-save-h t))))))

(defun org-roam-headline-db-autosync--delete-file-a (file &optional _trash)
  "Maintain cache consistency when file deletes.
FILE is removed from the database."
  (when (and (not (auto-save-file-name-p file))
             (not (backup-file-name-p file))
             (org-roam-file-p file))
    (org-roam-headline-db--clear-file (expand-file-name file))))

(defun org-roam-headline-db-autosync--rename-file-a (old-file new-file-or-dir &rest _args)
  "Maintain cache consistency of file rename.
OLD-FILE is cleared from the database, and NEW-FILE-OR-DIR is added."
  (let ((new-file (if (directory-name-p new-file-or-dir)
                      (expand-file-name (file-name-nondirectory old-file) new-file-or-dir)
                    new-file-or-dir)))
    (setq new-file (expand-file-name new-file))
    (setq old-file (expand-file-name old-file))
    (when (and (not (auto-save-file-name-p old-file))
               (not (auto-save-file-name-p new-file))
               (not (backup-file-name-p old-file))
               (not (backup-file-name-p new-file))
               (org-roam-file-p old-file))
      (org-roam-headline-db--clear-file old-file))
    (when (org-roam-file-p new-file)
      (emacsql-with-transaction (org-roam-headline-db)
		(org-roam-headline-db--insert-entry new-file)))))

(defun org-roam-headline-db-autosync--setup-update-on-save-h ()
  "Setup the current buffer if it visits an Org-roam file."
  (when (org-roam-file-p)
	(add-hook 'after-save-hook #'org-roam-headline-db--try-update-on-save-h nil t)))

(defun org-roam-headline-db--try-update-on-save-h ()
  "If appropriate, update the database for the current file after saving buffer."
  (when org-roam-headline-db-update-on-save (org-roam-headline-db--update-file)))

;;;###autoload
(defun consult-org-roam-headline ()
  (interactive)
  (consult--read
   (org-roam-headline-read--completions)
   :prompt "Go to heading: "
   :category 'org-roam-headline--node
   :sort nil
   :require-match t
   :preview-key nil
   :state (consult--jump-state)
   :group
   (lambda (cand transform)
	 (let* ((node (plist-get
				   (text-properties-at 0 cand)
				   'node))
			(title (org-roam-headline--node-title node)))
	   (if transform cand title)))
   :lookup #'consult-org-roam-headline-lookup))

(defun consult-org-roam-headline-lookup (selected candidates &rest _)
  (let* ((node (cdr (assoc selected candidates)))
		 (headline (org-roam-headline--node-headline node))
		 (file (org-roam-headline--node-file node))
		 (buffer (find-file-noselect file))
		 pos)
	(with-current-buffer buffer
	  (org-map-entries
	   (lambda ()
		 (when (string= (nth 4 (org-heading-components)) headline)
		   (setq pos (point-marker))))))
	pos))


;;;; provide
(provide 'org-roam-headline)
;;; org-roam-headline.el ends here.
