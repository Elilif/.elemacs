;; lib-appt.el --- Initialize lib-appt configurations.	-*- lexical-binding: t; -*-

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

;; 更新agenda时，同步appt
(defun eli/org-agenda-to-appt ()
  "call org-agenda-to-appt with refresh."
  (org-agenda-to-appt t))

(defun appt-disp-window-and-notification (min-to-appt current-time appt-msg)
  (if (atom min-to-appt)
	  (notifications-notify :timeout (* appt-display-interval 60000)
                            ;; 一直持续到下一次提醒
			                :title (format "%s分钟内有新的任务" min-to-appt)
			                :body appt-msg)
    (dolist (i (number-sequence 0 (1- (length min-to-appt))))
	  (notifications-notify :timeout (* appt-display-interval 60000)
                            ;; 一直持续到下一次提醒
			                :title (format "%s分钟内有新的任务"
                                           (nth i min-to-appt))
			                :body (nth i appt-msg)))))


;;;; provide
(provide 'lib-appt)
;;; lib-appt.el ends here.
