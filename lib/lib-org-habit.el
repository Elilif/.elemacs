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

(defun eli/org-habit-parse-todo (orig &rest args)
  (let ((org-habit-preceding-days 99999)
		(org-habit-following-days 99999))
	(apply orig args)))


(defun eli/habit-streaks (habit)
  (let ((closed-days (nth 4 habit))
	    (counter 1)
	    (sum (length (nth 4 habit)))
	    (streaks 1)
	    (current-streaks 0)
	    (today (time-to-days (current-time)))
	    (max-streaks 1))
    (while (< counter (length closed-days))
	  (if (= (time-convert (time-subtract (nth  counter closed-days)
                                          (nth (1- counter) closed-days))
                           'integer) 1)
	      (progn (setq streaks (1+ streaks)))
	    (if (> streaks max-streaks)
	        (progn (setq max-streaks streaks)
		           (setq streaks 1))))
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
		       ))
    (if (> streaks max-streaks)
	    (setq max-streaks streaks))
    (propertize (concat " ("
                        (number-to-string current-streaks)
                        "/"
                        (number-to-string max-streaks)
                        "/"
                        (number-to-string sum) ")")
                'face 'mindre-faded)))

(defun eli/org-habit-add-streak ()
  "Insert consistency graph for any habitual tasks."
  (mapc (lambda (ov)
		  (when (overlay-get ov 'after-string)
			(delete-overlay ov)))
		(overlays-in (point-min) (point-max)))
  (let ((buffer-invisibility-spec '(org-link)))
    (save-excursion
	  (goto-char (point-min))
	  (while (not (eobp))
	    (when-let ((habit (get-text-property (point) 'org-habit-p))
				   (pos (save-excursion
						  (search-forward (get-text-property (point) 'txt))))
				   (ov (make-overlay (1- pos) pos)))
		  (overlay-put ov 'after-string (eli/habit-streaks habit)))
	    (forward-line)))))


;;;; provide
(provide 'lib-org-habit)
;;; lib-org-habit.el ends here.
