;;; org-heatmap.el --- Show heatmap in calendar  -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs
;; Version: 0.1
;; Package-Requires: ((emacs "28.2") (org "9.6") (emacsql "3.1.1"))
;; Keywords: Calendar, Org, Habits
;; SPDX-License-Identifier: MIT

(require 'calendar)
(require 'org-habit)
(require 'emacsql-sqlite-builtin)

(defface org-heatmap-calendar-scale-1  '((((background light)) :foreground "black" :background "#c6e48b")
										 (((background dark))  :foreground "white" :background "#c6e48b")) "")
(defface org-heatmap-calendar-scale-2  '((((background light)) :foreground "black" :background "#7bc96f")
										 (((background dark))  :foreground "white" :background "#7bc96f")) "")
(defface org-heatmap-calendar-scale-3  '((((background light)) :foreground "black" :background "#239a3b")
										 (((background dark))  :foreground "white" :background "#239a3b")) "")
(defface org-heatmap-calendar-scale-4  '((((background light)) :foreground "black" :background "#196127")
										 (((background dark))  :foreground "white" :background "#196127")) "")

(defgroup org-heatmap nil
  "Settings for `org-heatmap'."
  :group 'org)

(defcustom org-heatmap-threshold '((default . ((0 . default)
											   (1 . org-heatmap-calendar-scale-1)
											   (3 . org-heatmap-calendar-scale-2)
											   (5 . org-heatmap-calendar-scale-3)
											   (7 . org-heatmap-calendar-scale-4)))
								   (habit . ((0 . org-heatmap-calendar-scale-1)
											 (15 . org-heatmap-calendar-scale-2)
											 (30 . org-heatmap-calendar-scale-3)
											 (60 . org-heatmap-calendar-scale-4))))
  "Choose a different face based on the quantity arrived."
  :group 'org-heatmap
  :type '(repeat (cons symbol (cons number symbol))))

(defcustom org-heatmap-get-threshold-function #'org-heatmap-get-threshold-defualt
  "Function used to get threshold."
  :group 'org-heatmap
  :type 'function)

(defcustom org-heatmap-db-location "~/.emacs.d/var/org/org-heatmap.db"
  "Default db locationl"
  :group 'org-heatmap
  :type 'directory)

(defvar org-heatmap-current-streak nil
  "Hash table used to store current streak.")

;;;; database
(defvar org-heatmap--db nil
  "Current connected org-heatmap database.")

(defun org-heatmap-db--close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for the database in
the current `org-heatmap-db-location'."
  (unless db
    (setq db org-heatmap--db))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)
	(setq org-heatmap--db nil)))

(defun org-heatmap-db--init (db table)
  "Initialize database DB with the correct schema and user version."
  (emacsql-with-transaction db
    (emacsql db [:create-table $s1 ([(date :primary-key)
									 (num :not-null)])]
			 table)))

(defun org-heatmap-db ()
  "Connect Org-heatmap database."
  (unless (and org-heatmap--db
			   (emacsql-live-p org-heatmap--db))
	(let ((init-db (not (file-exists-p org-heatmap-db-location))))
	  (make-directory (file-name-directory org-heatmap-db-location) t)
	  (let ((conn (emacsql-sqlite-builtin org-heatmap-db-location)))
		(emacsql conn [:pragma (= foreign_keys ON)])
		(when-let* ((process (emacsql-process conn))
                    ((processp process)))
          (set-process-query-on-exit-flag process nil))
		(when init-db
          (org-heatmap-db--init conn 'done-items))
		(setq org-heatmap--db conn))))
  org-heatmap--db)

(defun org-heatmap-db--query (sql &rest args)
  "Run SQL query on Org-heatmap database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (emacsql-with-transaction (org-heatmap-db)
	(apply #'emacsql org-heatmap--db sql args)))

(defun org-heatmap-db--init-done-items ()
  "Insert new record."
  (let* ((date (org-heatmap-today)))
	(org-heatmap-db--query [:insert :into $s1
									:values $v2]
						   'done-items
						   (vector date 1))))

(defun org-heatmap-db--query-date (d table)
  (org-heatmap-db--query [:select [date num]
								  :from $s1
								  :where (= date $s2)]
						 table d))

(defun org-heatmap-db--update-done-items (date num)
  (org-heatmap-db--query [:update $s1
								  :set (= num $s2)
								  :where (= date $s3)]
						 'done-items
						 num
						 date))

;;;; utilities
(defun org-heatmap-today ()
  (format-time-string "%Y-%m-%d" (current-time)))

(defun org-heatmap-calendar-get-date ()
  (org-heatmap-time-format (calendar-absolute-from-gregorian
							(calendar-cursor-to-date t))))

(defun org-heatmap-habit-update-p ()
  (and (org-is-habit-p)
	   (org-heatmap-db--table-exist-p (org-heatmap--hd-name))))

(defun org-heatmap-habit-p ()
  (or (org-is-habit-p)
	  (let* ((marker (or (org-get-at-bol 'org-marker)
						 (org-agenda-error)))
			 (buffer (marker-buffer marker))
			 (pos (marker-position marker)))
		(with-current-buffer buffer
		  (goto-char pos)
		  (org-is-habit-p)))))

(defun org-heatmap-update-counter ()
  (when (string= "DONE" (org-get-todo-state))
	(let ((td (org-heatmap-today))
		  (hd-name (org-heatmap--hd-name)))
	  (cond
	   ((org-heatmap-habit-update-p)
		(let ((time (save-excursion
					  (save-restriction
						(org-narrow-to-subtree)
						(org-heatmap-clock-sum td)))))
		  (org-heatmap-db--query [:insert :into $s1
										  :values $v2]
								 hd-name
								 (vector td time))))
	   (t (if-let* ((result (cadar (org-heatmap-db--query-date td 'done-items))))
			  (org-heatmap-db--update-done-items td (1+ result))
			(org-heatmap-db--init-done-items)))))))

(defun org-heatmap-time-format (days)
  (let ((date (calendar-gregorian-from-absolute days)))
	(format "%04d-%02d-%02d"
			(nth 2 date) 
			(nth 0 date) 
			(nth 1 date))))

(defun org-heatmap-get-streak (table)
  (let ((streak (org-heatmap-db--query [:select [date num]
												:from $s1]
									   table))
		(table (make-hash-table :test #'equal))
		(type (if (eq table 'done-items)
				  'default
				'habit)))
	(dolist (item streak)
      (puthash (car item) (cadr item) table))
	(setq org-heatmap-current-streak (cons type table))))

(defun org-heatmap-get-threshold-defualt (n)
  (cdr
   (cl-find-if (lambda (pair)
				 (>= n (car pair)))
			   (reverse (alist-get (car org-heatmap-current-streak)
								   org-heatmap-threshold)))))

(defun org-heatmap-generate (month year _indent)
  (when org-heatmap-current-streak
    (dotimes (i 31)
	  (let ((date (list month (1+ i) year))
            (count-scaled (gethash (format "%04d-%02d-%02d" year month (1+ i))
								   (cdr org-heatmap-current-streak))))
        (when count-scaled
		  (calendar-mark-visible-date
		   date
		   (funcall org-heatmap-get-threshold-function count-scaled)))))))

(defun org-heatmap-clear (&rest _args)
  (setq org-heatmap-current-streak nil))

(defun org-heatmap-clock-sum (date)
  (let* ((cc (org-clock-special-range date))
		 (ts (car cc))
		 (te (nth 1 cc)))
	(org-heatmap-clock-sum-1 ts te)))

(defun org-heatmap-clock-sum-1 (tstart tend)
  (let* ((re (concat "^[ \t]*"
					 org-clock-string
					 "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
		 (tstart (float-time tstart))
		 (tend (float-time tend))
		 (sum 0))
	(save-excursion
	  (goto-char (point-max))
	  (while (re-search-backward re nil t)
		(let* ((ss (match-string 1))
			   (se (match-string 2))
			   (ts (org-time-string-to-seconds ss))
		       (te (org-time-string-to-seconds se))
			   (dt (- (if tend (min te tend) te)
					  (if tstart (max ts tstart) ts))))
		  (when (> dt 0) (cl-incf sum (floor dt 60))))))
	sum))

(defun org-heatmap--hd-name ()
  (cond
   ((eq major-mode 'org-agenda-mode)
	(let ((todo-state (get-text-property (point) 'todo-state))
		  (txt (get-text-property (point) 'txt)))
	  (substring-no-properties txt (1+ (length todo-state)))))
   (t (nth 4 (org-heading-components)))))

(defun org-heatmap-habit-parse-todo ()
  (let ((org-habit-preceding-days 99999)
		(org-habit-following-days 99999))
	(reverse (nth 4 (org-habit-parse-todo)))))

(defun org-heatmap-habit--collect (&rest _args)
  (let* ((marker (or (org-get-at-bol 'org-marker)
					 (org-agenda-error)))
		 (buffer (marker-buffer marker))
		 (pos (marker-position marker))
		 (hd-name (org-heatmap--hd-name))
		 closed-dates)
	(unless buffer
	  (user-error "Trying to switch to non-existent buffer"))
	(with-current-buffer buffer
	  (save-excursion
		(save-restriction
		  (goto-char pos)
		  (org-back-to-heading t)
		  (setq closed-dates (org-heatmap-habit-parse-todo))
		  (org-narrow-to-subtree)
		  (org-heatmap-db--init (or org-heatmap--db
									(emacsql-sqlite-builtin org-heatmap-db-location))
								hd-name)
		  (dolist (closed-date closed-dates) 
			(let ((date (org-heatmap-time-format closed-date)))
			  (org-heatmap-db--query [:insert :into $s1
											  :values $v2]
									 hd-name
									 (vector date (org-heatmap-clock-sum date))))
			(sleep-for 0.01)))))))

(defun org-heatmap-db--table-exist-p (table)
  (emacsql-with-transaction (org-heatmap-db) 
	(emacsql org-heatmap--db [:select name :from sqlite_master
									  :where (and (= type 'table) (= name $s1))]
			 table)))

;;;; interactive functions

;;;###autoload
(defun org-heatmap-habit-calendar ()
  (interactive)
  (let ((hd-name (org-heatmap--hd-name)))
	(if (org-heatmap-habit-p)
		(progn
		  (unless (org-heatmap-db--table-exist-p hd-name)
			(org-heatmap-habit--collect))
		  (org-heatmap-get-streak hd-name)
		  (calendar))
	  (error "Not on a habit!"))))

;;;###autoload
(defun org-heatmap-calendar ()
  (interactive)
  (org-heatmap-get-streak 'done-items)
  (calendar))

;;;###autoload
(defun org-heatmap-calendar-query ()
  (interactive)
  (if (eq major-mode 'calendar-mode)
	  (let* ((date (org-heatmap-calendar-get-date))
			 (tasks (gethash date (cdr org-heatmap-current-streak))))
		(message "%d %s in %s"
				 (if (numberp tasks) tasks 0)
				 (if (eq (car org-heatmap-current-streak) 'default)
					 "items are done"
				   "minutes are spent")
				 date))
	(error "Must be used in calendar mode!")))

;;;###autoload
(defun org-heatmap-adjust ()
  (interactive)
  (if (and (eq major-mode 'calendar-mode)
		   (eq (car org-heatmap-current-streak) 'default))
	  (if-let* ((date (org-heatmap-calendar-get-date))
				(tasks (gethash date (cdr org-heatmap-current-streak)))
				(num (read-number
					  (format "Input a num(current: %d): " tasks))))
		  (progn
			(org-heatmap-db--update-done-items date num)
			(puthash date num (cdr org-heatmap-current-streak))
			(mapc #'delete-overlay (overlays-in (1- (point)) (1+ (point))))
			(calendar-mark-visible-date
			 (calendar-cursor-to-date t)
			 (funcall org-heatmap-get-threshold-function
					  (gethash date (cdr org-heatmap-current-streak)))))
		(error "%s hasn't any record!" date))
	(error "Must be used with default streak in calendar mode!")))

;;;###autoload
(define-minor-mode org-heatmap-mode
  "Show heatmap in calendar."
  :global t
  :group 'org-heatmap
  (cond
   (org-heatmap-mode
	(advice-add #'calendar-exit :after #'org-heatmap-clear)
	(advice-add #'calendar-generate-month :after #'org-heatmap-generate)
	(add-hook 'kill-emacs-hook #'org-heatmap-db--close)
	(add-hook 'org-after-todo-state-change-hook #'org-heatmap-update-counter)
	(keymap-set calendar-mode-map "j" #'org-heatmap-adjust)
	(keymap-set calendar-mode-map "f" #'org-heatmap-calendar-query))
   (t
	(org-heatmap-db--close)
	(advice-remove #'calendar-exit #'org-heatmap-clear)
	(advice-remove #'calendar-generate-month #'org-heatmap-generate)
	(remove-hook 'org-after-todo-state-change-hook #'org-heatmap-update-counter)
	(remove-hook 'kill-emacs-hook #'org-heatmap-db--close)
	(keymap-set calendar-mode-map "j" nil)
	(keymap-set calendar-mode-map "f" nil))))

(provide 'org-heatmap)
