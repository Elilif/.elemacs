;;; org-matable.el --- insert matrices quickly -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

;; Version: 0.1
;; Package-Requires: ((emacs "29.0.9") (org "0.6.2"))
;; Keywords: LaTeX, org-table
;; SPDX-License-Identifier: GPL-3.0-or-later

(defun org-matable-at-table-p (&rest _arg)
  (org-match-line "[ \t]*|"))

(defun org-matable-get-size ()
  "Return the size of the org table at point as a cons cell, (ROWS . COLS).
Move to the start of table if needed. Return nil if not at a table."
  (when (org-at-table-p)
    (save-excursion
      (goto-char (org-table-end))
	  (re-search-backward "|")
      (let ((row (org-table-current-line))
            (col (org-table-current-column)))
        (cons row col)))))

(defun org-matable-dots-update ()
  (let* ((size (org-matable-get-size))
		 (max-col (cdr size))
		 (max-line (car size))
		 (times (- max-col (org-table-current-column))))
	(save-excursion
	  (dotimes (line max-line)
		(org-table-goto-line (1+ line))
		(org-table-goto-column max-col)
		(dotimes (_ times)
		  (let* ((diff (number-to-string (- (org-table-current-column) max-col)))
				 (default-field (org-table-blank-field))
				 (sub (progn
						(string-match "{\\(.*?\\)}" default-field)
						(match-string 1 default-field))))
			(insert (if (string-match "{.*?,\\(.*?\\)}" default-field)
						(replace-regexp-in-string
						 "{.*?,\\(.*?\\)}"
						 (concat
						  "m"
						  (if (string= diff "0") "" diff))
						 default-field
						 nil nil 1)
					  (replace-regexp-in-string
					   "{\\(.*?\\)}"
					   (concat
						(when (length> sub 1)
						  (concat
						   (number-to-string (org-table-current-line))
						   ","))
						"m"
						(if (string= diff "0") "" diff))
					   default-field
					   nil nil 1))))
		  (re-search-backward " |" (line-beginning-position) 'noerror))))))

(defun org-matable-vdots-update ()
  (let* ((size (org-matable-get-size))
		 (max-col (cdr size))
		 (max-line (car size))
		 (times (- max-line (org-table-current-line))))
	(save-excursion
	  (dotimes (col max-col)
		(org-table-goto-line max-line)
		(org-table-goto-column (1+ col))
		(dotimes (_ times)
		  (let* ((cur-line (org-table-current-line))
				 (diff (number-to-string (- cur-line max-line)))
				 (default-field (org-table-blank-field))
				 (sub (progn
						(string-match "{\\(.*?\\)}" default-field)
						(match-string 1 default-field))))
			(save-excursion
			  (insert
			   (if (string-match "{\\(.*?\\),.*?}" default-field)
				   (replace-regexp-in-string
					"{\\(.*?\\),.*?}"
					(concat
					 "n"
					 (if (string= diff "0") "" diff))
					default-field
					nil nil 1)
				 (replace-regexp-in-string
				  "{\\(.*?\\)}"
				  (concat
				   "n"
				   (if (string= diff "0") "" diff)
				   (when (length> sub 1)
					 (concat
					  ","
					  (number-to-string (org-table-current-column)))))
				  default-field
				  nil nil 1))))
			(org-table-goto-line (1- cur-line))
			(org-table-goto-column (1+ col))))))))

(defmacro org-matable-fill-column (&rest body)
  (declare (indent defun))
  `(let* ((lines (car (org-matable-get-size)))
		  (current-tbl-col (org-table-current-column))
		  (current-pos-col (current-column)))
	 (org-table-goto-line 1)
	 (org-table-goto-column current-tbl-col)
	 (dotimes (line lines)
	   ,@body
	   (move-to-column current-pos-col)
	   (when (< (1+ line) lines)
		 (next-line)))
	 (org-table-align)
	 (goto-char (org-table-begin))))

(defmacro org-matable-fill-line (&rest body)
  (declare (indent defun))
  `(let ((cols (cdr (org-matable-get-size))))
	 (org-table-goto-column 1)
	 (dotimes (_ cols)
	   ,@body
	   (re-search-forward "| " (line-end-position) 'noerror))
	 (org-table-align)
	 (goto-char (org-table-begin))))

;;;###autoload
(defun org-matable-fill ()
  (interactive)
  (save-excursion
	(let* ((end-marker (save-excursion
						 (goto-char (1- (org-table-end)))
						 (point-marker)))
		   (default-field (string-clean-whitespace
						   (org-table-get-field 1))))
	  (while (and (re-search-forward "| " end-marker 'noerror)
				  (< (point) end-marker))
		(insert (replace-regexp-in-string
				 "{\\(.*?\\)}"
				 (concat
				  (number-to-string (org-table-current-line))
				  (number-to-string (org-table-current-column)))
				 default-field
				 nil nil 1)))))
  (org-table-align))

;;;###autoload
(defun org-matable-insert-dots (arg)
  (interactive "P")
  (unless arg
	(org-matable-dots-update))
  (org-matable-fill-column
	(if (string-match-p "dot" (org-table-blank-field))
		(insert "\\ddots")
	  (insert "\\dots"))))


;;;###autoload
(defun org-matable-insert-vdots (arg)
  (interactive "P")
  (unless arg
	(org-matable-vdots-update))
  (org-matable-fill-line
	(if (string-match-p "dot" (org-table-blank-field))
		(insert "\\ddots")
	  (insert "\\vdots"))))

;;;###autoload
(defun org-matable-inesrt-column ()
  (interactive)
  (let ((default-fild (org-table-blank-field)))
	(org-matable-fill-column
	  (insert (replace-regexp-in-string
			   "{\\(.*?\\)}"
			   (number-to-string (org-table-current-line))
			   default-fild
			   nil nil 1)))))

;;;###autoload
(defun org-matable-inesrt-line ()
  (interactive)
  (let ((default-fild (org-table-blank-field)))
	(org-matable-fill-line
	  (insert (replace-regexp-in-string
			   "{\\(.*?\\)}"
			   (number-to-string (org-table-current-column))
			   default-fild
			   nil nil 1)))))

;;; SRC: https://github.com/karthink/lazytab/tree/master
(defun org-matable-orgtbl-to-amsmath (table params)
  (orgtbl-to-generic
   table
   (org-combine-plists
    '(:splice t
			  :lstart ""
			  :lend " \\\\"
			  :sep " & "
			  :hline nil
			  :llend "")
    params)))

;;;###autoload
(defun org-matable-orgtbl-replace (arg)
  (interactive "P")
  (goto-char (org-table-begin))
  (let* ((table (org-table-to-lisp))
		 (params '(:backend latex :raw t))
		 (replacement-table
		  (if (not arg)
			  (org-matable-orgtbl-to-amsmath table params)
			(orgtbl-to-latex table params))))
	(kill-region (org-table-begin) (org-table-end))
	(open-line 1)
	(push-mark)
	(insert replacement-table)
	(align-regexp (region-beginning) (region-end) "\\(\\s-*\\) &" 1 0 t)
	(org-matable-mode -1)))

;;;###autoload
(defun org-matable-create (&optional size read-row)
  "Query for a size and insert a table skeleton.
SIZE is a string Rows x Columns like for example \"3 2\"."
  (interactive)
  (unless size
    (setq size (read-string
				(concat "Table size Rows x Columns [default: 5 5]: ")
				"" nil "5 5")))
  (let* ((indent (make-string (current-column) ?\ ))
		 (split (org-split-string size " "))
		 (columns (string-to-number (if (length= split 1)
										(car split)
									  (nth 1 split))))
		 (rows (if read-row
				   (read-number "Rows: ")
				 (if (length= split 1)
					 columns
				   (string-to-number (car split)))))
		 (line (concat (apply 'concat indent "|" (make-list columns "  |"))
					   "\n")))
    (if (string-match "^[ \t]*$" (buffer-substring-no-properties
                                  (line-beginning-position) (point)))
		(beginning-of-line 1)
      (newline))
    (save-excursion
	  (dotimes (_ rows) (insert line))
	  (delete-line))
    (org-table-align))
  (org-matable-mode 1))

(defvar org-matable-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-c" 'org-matable-orgtbl-replace)
    (keymap-set map "s-v" 'org-matable-insert-vdots)
	(keymap-set map "s-f" 'org-matable-fill)
	(keymap-set map "s-c" 'org-table-insert-column)
	(keymap-set map "s-n" 'org-matable-inesrt-column)
	(keymap-set map "s-l" 'org-matable-inesrt-line)
	(keymap-set map "s-d" 'org-matable-insert-dots)
	(keymap-set map "TAB" #'org-table-next-field)
    map)
  "Parent keymap for all keymaps of modes derived from `org-matable-mode'.")

;;;###autoload
(define-minor-mode org-matable-mode
  "Minor mode for fast matrices insertion."
  :global t
  (cond
   (org-matable-mode
	(advice-add 'texmathp :override #'org-matable-at-table-p)
	(advice-add 'org-at-table-p :override #'org-matable-at-table-p)
	(advice-add 'org-inside-LaTeX-fragment-p :override #'org-at-table-p))
   (t
	(advice-remove 'texmathp #'org-matable-at-table-p)
	(advice-remove 'org-at-table-p #'org-matable-at-table-p)
	(advice-remove 'org-inside-LaTeX-fragment-p #'org-at-table-p))))

(provide 'org-matable)
;;; org-matable.el ends here
