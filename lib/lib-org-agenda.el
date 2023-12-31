;; lib-org-agenda.el --- Initialize lib-org-agenda configurations.	-*- lexical-binding: t; -*-

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

(defun eli/org-agenda-goto-started-task (orig)
  (org-agenda-redo)
  (goto-char (point-min))
  (if (bound-and-true-p org-clock-current-task)
      (if (not (save-excursion
                 (re-search-forward org-clock-current-task nil t)))
          (progn
            (re-search-forward "^started" nil t)
            (recenter-top-bottom 1)
            (forward-line))
        (funcall orig))
    (progn
      (re-search-forward "^started" nil t)
      (recenter-top-bottom 1)
      (forward-line))))

(defun eli/make-progress (width)
  (let* ((today (time-to-day-in-year (current-time)))
         (percent (floor (* 100 (/ today 365.0))))
         (done (/ percent 100.0))
         (done-width (floor (* width done))))
    (concat
     "["
     (make-string done-width ?/)
     (make-string (- width done-width) ? )
     "]"
     (concat " " (number-to-string percent) "%"))))

(defun eli/make-svg-progress ()
  (let* ((today (time-to-day-in-year (current-time)))
         (percent (/ today 365.0))
         (image-scaling-factor 2.0))
    (concat
     (propertize
      " "
      'display
      (svg-lib-progress-bar percent nil
                            :margin 0 :stroke 2 :radius 3
                            :padding 2 :width 18 :height 0.6
                            :foreground "#B0BEC5"))
     (concat " " (number-to-string (floor (* 100 percent))) "%"))))

;; change the progress color
(defun eli/show-progress-color ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\\([0-9]\\{2,3\\}\\)%" nil t)
      (let* ((percent (string-to-number (match-string 1)))
             percent-face)
        (cond
         ((< percent 33)
          (setq percent-face 'mindre-note))
         ((< percent 66)
          (setq percent-face 'mindre-keyword))
         ((< percent 90)
          (setq percent-face 'mindre-warning))
         ((<= percent 100)
          (setq percent-face 'mindre-critical)))
        (overlay-put (make-overlay
                      (match-beginning 0) (match-end 0))
                     'face percent-face)))))

(defvar dynamic-agenda-files nil
  "dynamic generate agenda files list when changing org state")

(defun update-dynamic-agenda-hook ()
  (let* ((org-state (org-get-todo-state))
		 (done (or (not org-state) ;; nil when no TODO list
                   (member org-state org-done-keywords)))
         (file (buffer-file-name))
         (agenda (funcall (ad-get-orig-definition 'org-agenda-files)))
         (custom-file (expand-file-name "custom.el" user-emacs-directory)))
    (unless (or (member file agenda)
                (not file))
	  (if done
          (save-excursion
            (goto-char (point-min))
            ;; Delete file from dynamic files
            ;; when all TODO entry changed to DONE
            (unless (search-forward-regexp org-not-done-heading-regexp nil t)
		      (customize-save-variable
		       'dynamic-agenda-files
		       (cl-delete-if (lambda (k) (string= k file))
			                 dynamic-agenda-files))))
        ;; Add this file to dynamic agenda files
        (unless (member file dynamic-agenda-files)
          (customize-save-variable 'dynamic-agenda-files
                                   (add-to-list 'dynamic-agenda-files
                                                file)))))))

(defun dynamic-agenda-files-advice (orig-val)
  (cl-union orig-val dynamic-agenda-files :test #'equal))


;; from: https://emacs.stackexchange.com/questions/31683
;; /schedule-org-task-for-last-day-of-every-month
;; ORG-MODE:  * My Task
;;              SCHEDULED: <%%(diary-last-day-of-month date)>
;; DIARY:  %%(diary-last-day-of-month date) Last Day of the Month
;; See also:  (setq org-agenda-include-diary t)
;; (diary-last-day-of-month '(2 28 2017))
(defun diary-last-day-of-month (date)
  "Return `t` if DATE is the last day of the month."
  (let* ((day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-day-of-month
          (calendar-last-day-of-month month year)))
    (= day last-day-of-month)))


;;;; provide
(provide 'lib-org-agenda)
;;; lib-org-agenda.el ends here.
