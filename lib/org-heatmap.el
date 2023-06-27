;;; org-heatmap.el --- Show heatmap in calendar  -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs
;; Version: 0.1
;; Package-Requires: ((emacs "28.2") (org "9.6") (emacsql "3.1.1"))
;; Keywords: Calendar, Org, Habits
;; SPDX-License-Identifier: MIT

(require 'calendar)
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

(defcustom org-heatmap-threshold '((0 . default)
								   (1 . org-heatmap-calendar-scale-1)
								   (3 . org-heatmap-calendar-scale-2)
								   (5 . org-heatmap-calendar-scale-3)
								   (7 . org-heatmap-calendar-scale-4))
  "Choose a different face based on the quantity arrived."
  :group 'org-heatmap
  :type '(repeat (cons number symbol)))

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
    (emacsql-close db)))

(defun org-heatmap-db--init (db)
  "Initialize database DB with the correct schema and user version."
  (emacsql-with-transaction db
    (emacsql db [:create-table heatmap ([(date :primary-key)
										 (done-tasks :not-null)])])))

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
          (org-heatmap-db--init conn))
		(setq org-heatmap--db conn))))
  org-heatmap--db)

(defun org-heatmap-db--query (sql &rest args)
  "Run SQL query on Org-heatmap database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (emacsql-with-transaction (org-heatmap-db)
	(apply #'emacsql org-heatmap--db sql args)))

(defun org-heatmap-db--init-entry ()
  "Insert new record."
  (let* ((date (org-heatmap-today)))
	(org-heatmap-db--query [:insert :into heatmap
									:values $v1]
						   (vector date 1))))

(defun org-heatmap-db-query--date (d)
  (org-heatmap-db--query [:select [date done-tasks]
								  :from heatmap
								  :where (= date $s1)]
						 d))

(defun org-heatmap-db-update (date num)
  (org-heatmap-db--query [:update heatmap
								  :set (= done-tasks $s1)
								  :where (= date $s2)]
						 num
						 date))

;;;; utilities
(defun org-heatmap-today ()
  (format-time-string "%Y-%m-%d" (current-time)))

(defun org-heatmap-calendar-get-date ()
  (org-heatmap-time-format (calendar-absolute-from-gregorian
							(calendar-cursor-to-date t))))

(defun org-heatmap-update-counter ()
  (when (string= "DONE" (org-get-todo-state))
	(if-let* ((td (org-heatmap-today))
			  (result (1+ (cadar (org-heatmap-db-query--date td)))))
		(org-heatmap-db-update td result)
	  (org-heatmap-db--init-entry))))

(defun org-heatmap-time-format (days)
  (let ((date (calendar-gregorian-from-absolute days)))
	(format "%04d-%02d-%02d"
			(nth 2 date) 
			(nth 0 date) 
			(nth 1 date))))

(defun org-heatmap-get-habit-streak ()
  (let ((closed-dates (or (nth 4 (get-text-property (point) 'org-habit-p))
						  (org-agenda-error)))
		(sums (make-hash-table :test #'equal)))
	(dolist (closed-date closed-dates)
	  (puthash (org-heatmap-time-format closed-date) 3 sums))
	(setq org-heatmap-current-streak sums)))

(defun org-heatmap-get-done-streak ()
  (let ((streak (org-heatmap-db--query [:select [date done-tasks]
												:from heatmap]))
		(table (make-hash-table :test #'equal)))
	(dolist (item streak)
      (puthash (car item) (cadr item) table))
	(setq org-heatmap-current-streak table)))

(defun org-heatmap-get-threshold-defualt (n)
  (cdr
   (cl-find-if (lambda (pair)
				 (>= n (car pair)))
			   (reverse org-heatmap-threshold))))

(defun org-heatmap-generate (month year _indent)
  (when org-heatmap-current-streak
    (dotimes (i 31)
	  (let ((date (list month (1+ i) year))
            (count-scaled (gethash (format "%04d-%02d-%02d" year month (1+ i))
								   org-heatmap-current-streak)))
        (when count-scaled
		  (calendar-mark-visible-date
		   date
		   (funcall org-heatmap-get-threshold-function count-scaled)))))))

(defun org-heatmap-clear (&rest _args)
  (setq org-heatmap-current-streak nil))

;;;###autoload
(defun org-heatmap-habit-calendar ()
  (interactive)
  (org-heatmap-get-habit-streak)
  (org-agenda-goto-calendar))

;;;###autoload
(defun org-heatmap-calendar ()
  (interactive)
  (org-heatmap-get-done-streak)
  (calendar))

;;;###autoload
(defun org-heatmap-calendar-query ()
  (interactive)
  (if (eq major-mode 'calendar-mode)
	  (let* ((date (org-heatmap-calendar-get-date))
			 (tasks (cadar (org-heatmap-db-query--date date))))
		(message "%d tasks are done in %s" (or tasks 0) date))
	(error "Must be used in calendar mode!")))

;;;###autoload
(defun org-heatmap-adjust (num)
  (interactive "nInput a number: ")
  (if (eq major-mode 'calendar-mode)
	  (let* ((date (org-heatmap-calendar-get-date)))
		(org-heatmap-db-update date num))
	(error "Must be used in calendar mode!")))

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

;; (defun org-heatmap-clock-sum (date)
;;   (let* ((cc (org-clock-special-range date))
;; 		 (ts (car cc))
;; 		 (te (nth 1 cc)))
;; 	(org-clock-sum ts te)))

;; (defun org-heatmap-get-clock-sum ()
;;   (let* ((closed-dates (or (nth 4 (get-text-property (point) 'org-habit-p))
;; 						   (org-agenda-error)))
;; 		 (sums (make-hash-table :test #'equal))
;; 		 (marker (or (org-get-at-bol 'org-marker)
;; 					 (org-agenda-error)))
;; 		 (buffer (marker-buffer marker))
;; 		 (pos (marker-position marker)))
;; 	(unless buffer
;; 	  (user-error "Trying to switch to non-existent buffer"))
;; 	(with-current-buffer buffer
;; 	  (save-excursion
;; 		(save-restriction
;; 		  (goto-char pos)
;; 		  (org-back-to-heading t)
;; 		  (org-narrow-to-subtree)
;; 		  (dolist (closed-date closed-dates)
;; 			(let ((date (org-heatmap-time-format closed-date)))
;; 			  (puthash date (org-heatmap-clock-sum date) sums))))))
;; 	(setq org-heatmap-current-streak sums)))

(provide 'org-heatmap)
