;; lib-ledger-mode.el --- Initialize lib-ledger-mode configurations.	-*- lexical-binding: t; -*-

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

(defun eli/ledger-read-date (prompt)
  "Advice for `ledger-read-date'."
  (let* ((prev-xact-time (or (ledger-xact-date)
							 (save-excursion
							   (ledger-navigate-prev-xact-or-directive)
							   (ledger-xact-date))))
		 (prev-xact-time-plus (ledger-format-date
							   (time-add
								(ledger-parse-iso-date prev-xact-time)
								(days-to-time 1))))
		 (default-input (cond
						 ((= (prefix-numeric-value current-prefix-arg) 4)
						  (string-replace "/" "-" prev-xact-time))
						 ((= (prefix-numeric-value current-prefix-arg) 16)
						  (string-replace "/" "-" prev-xact-time-plus))
						 (nil))))
	(ledger-format-date (let ((org-read-date-prefer-future nil))
						  (org-read-date nil t nil prompt nil default-input)))))

(defun eli/ledger-set-tab-style ()
  (setq-local tab-always-indent 'complete
			  completion-cycle-threshold t
			  ledger-complete-in-steps t))

;;;###autoload
(defun eli/jump-to-amount ()
  (interactive)
  (ledger-navigate-beginning-of-xact)
  (search-forward "¥" nil t))


;;;; provide
(provide 'lib-ledger-mode)
;;; lib-ledger-mode.el ends here.
