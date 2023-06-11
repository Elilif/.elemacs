;; lib-org-habit.el --- Initialize lib-org-habit configurations.	-*- lexical-binding: t; -*-

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

(defun org-habit-parse-todo (&optional pom)
  "Parse the TODO surrounding point for its habit-related data.
Returns a list with the following elements:

  0: Scheduled date for the habit (may be in the past)
  1: \".+\"-style repeater for the schedule, in days
  2: Optional deadline (nil if not present)
  3: If deadline, the repeater for the deadline, otherwise nil
  4: A list of all the past dates this todo was mark closed
  5: Repeater type as a string

This list represents a \"habit\" for the rest of this module."
  (save-excursion
    (if pom (goto-char pom))
    (cl-assert (org-is-habit-p (point)))
    (let* ((scheduled (org-get-scheduled-time (point)))
	       (scheduled-repeat (org-get-repeat (org-entry-get (point)
                                                            "SCHEDULED")))
	       (end (org-entry-end-position))
	       (habit-entry (org-no-properties (nth 4 (org-heading-components))))
	       closed-dates deadline dr-days sr-days sr-type)
	  (if scheduled
	      (setq scheduled (time-to-days scheduled))
	    (error "Habit %s has no scheduled date" habit-entry))
	  (unless scheduled-repeat
	    (error
	     "Habit `%s' has no scheduled repeat period or has an incorrect one"
	     habit-entry))
	  (setq sr-days (org-habit-duration-to-days scheduled-repeat)
	        sr-type (progn (string-match "[\\.+]?\\+" scheduled-repeat)
			               (match-string-no-properties 0 scheduled-repeat)))
	  (unless (> sr-days 0)
	    (error "Habit %s scheduled repeat period is less than 1d" habit-entry))
	  (when (string-match "/\\([0-9]+[dwmy]\\)" scheduled-repeat)
	    (setq dr-days (org-habit-duration-to-days
			           (match-string-no-properties 1 scheduled-repeat)))
	    (if (<= dr-days sr-days)
	        (error "Habit %s deadline repeat period is less than
or equal to scheduled (%s)"
		           habit-entry scheduled-repeat))
	    (setq deadline (+ scheduled (- dr-days sr-days))))
	  (org-back-to-heading t)
	  (let* ((maxdays 99999)
	         (reversed org-log-states-order-reversed)
	         (search (if reversed 're-search-forward 're-search-backward))
	         (limit (if reversed end (point)))
	         (count 0)
	         (re (format
		          "^[ \t]*-[ \t]+\\(?:State \"%s\".*%s%s\\)"
		          (regexp-opt org-done-keywords)
		          org-ts-regexp-inactive
		          (let ((value (cdr (assq 'done org-log-note-headings))))
		            (if (not value) ""
			          (concat "\\|"
				              (org-replace-escapes
				               (regexp-quote value)
				               `(("%d" . ,org-ts-regexp-inactive)
				                 ("%D" . ,org-ts-regexp)
				                 ("%s" . "\"\\S-+\"")
				                 ("%S" . "\"\\S-+\"")
				                 ("%t" . ,org-ts-regexp-inactive)
				                 ("%T" . ,org-ts-regexp)
				                 ("%u" . ".*?")
				                 ("%U" . ".*?")))))))))
	    (unless reversed (goto-char end))
	    (while (and (< count maxdays) (funcall search re limit t))
	      (push (time-to-days
		         (org-time-string-to-time
		          (or (match-string-no-properties 1)
			          (match-string-no-properties 2))))
		        closed-dates)
	      (setq count (1+ count))))
	  (list scheduled sr-days deadline dr-days closed-dates sr-type))))

;;;###autoload
(defun eli/habit-streaks (habit)
  (interactive)
  (let ((closed-days (nth 4 habit))
	    (counter 1)
	    (sum (length (nth 4 habit)))
	    (streaks 1)
	    (current-streaks 0)
	    (today (time-to-days (current-time)))
	    (max-streaks 1)
	    )
    (while (< counter (length closed-days))
	  (if (= (time-convert (time-subtract (nth  counter closed-days)
                                          (nth (1- counter) closed-days))
                           'integer) 1)
	      (progn (setq streaks (1+ streaks)))
	    (if (> streaks max-streaks)
	        (progn (setq max-streaks streaks)
		           (setq streaks 1)))
	    )
	  (setq counter (1+ counter)))
    (setq counter (1- counter))
    (if (= (time-convert (time-subtract today (nth counter closed-days))
                         'integer) 1)
	    (progn (setq current-streaks (1+ current-streaks))
		       (while (= (time-convert (time-subtract
                                        (nth  counter closed-days)
                                        (nth (1- counter) closed-days))
                                       'integer) 1)
		         (setq current-streaks (1+ current-streaks))
		         (setq counter (1- counter)))
		       )

	  )
    (if (> streaks max-streaks)
	    (setq max-streaks streaks))
    (insert (propertize (propertize (concat " ("
                                            (number-to-string current-streaks)
                                            "/"
                                            (number-to-string max-streaks)
                                            "/"
                                            (number-to-string sum) ")")
                                    'face 'mindre-faded)
                        'field t))))

(defun org-habit-insert-consistency-graphs (&optional line)
  "Insert consistency graph for any habitual tasks."
  (let ((inhibit-read-only t)
	    (buffer-invisibility-spec '(org-link))
	    (moment (org-time-subtract nil
				                   (* 3600 org-extend-today-until))))
    (save-excursion
	  (goto-char (if line (line-beginning-position) (point-min)))
	  (while (not (eobp))
	    (let ((habit (get-text-property (point) 'org-habit-p)))
	      (when habit
	        (move-to-column org-habit-graph-column t)
	        (delete-char (min (+ 1 org-habit-preceding-days
				                 org-habit-following-days)
				              (- (line-end-position) (point))))
	        (insert-before-markers
	         (org-habit-build-graph
		      habit
		      (time-subtract moment (days-to-time org-habit-preceding-days))
		      moment
		      (time-add moment (days-to-time org-habit-following-days))))
	        (end-of-line)
	        (eli/habit-streaks habit)
	        ))
	    (forward-line)))))


;;;; provide
(provide 'lib-org-habit)
;;; lib-org-habit.el ends here.
