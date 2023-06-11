;; lib-rime.el --- Initialize lib-rime configurations.	-*- lexical-binding: t; -*-

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

(defun +rime-predicate-punctuation-line-begin-p ()
  "Enter half-width punctuation at the beginning of the line.
  Detect whether the current cursor is at the beginning of a
  line and the character last inputted is symbol.
  Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (<= (point) (save-excursion (back-to-indentation) (point))))

(defun rime-predicate-after-latin-char-p ()
  "If the cursor is after a latin character.
Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
	   (let ((string (buffer-substring (point) (max (line-beginning-position)
                                                    (- (point) 80)))))
         (and (string-match-p "\\cl$" string)
              (not (string-match-p " $" string))))))

(defvar eli/prefer-English t)

;;;###autoload
(defun eli/input-switch ()
  (interactive)
  (if (not eli/prefer-English)
	  (progn
        (add-to-list 'rime-disable-predicates
                     'rime-predicate-space-after-ascii-p)
		(add-to-list 'rime-disable-predicates
                     '+rime-predicate-punctuation-line-begin-p)
	    (setq eli/prefer-English t))
    (progn
	  (setq rime-disable-predicates
	        (seq-difference rime-disable-predicates
                            '(rime-predicate-space-after-ascii-p
							  +rime-predicate-punctuation-line-begin-p)))
	  (setq eli/prefer-English nil)
	  )))

;;;###autoload
(defun +rime-convert-string-at-point (&optional _return-cregexp)
  "将光标前的字符串转换为中文."
  (interactive "P")
  (unless current-input-method
	(toggle-input-method))
  (rime-force-enable)
  (let ((string (if mark-active
                    (buffer-substring-no-properties
		             (region-beginning) (region-end))
                  (buffer-substring-no-properties
                   (point) (max (line-beginning-position) (- (point) 80)))))
        code
        length)
    (cond ((string-match "\\([a-z'-]+\\|[[:punct:]]\\) *$" string)
           (setq code (replace-regexp-in-string
			           "^[-']" ""
			           (match-string 0 string)))
           (setq length (length code))
           (setq code (replace-regexp-in-string " +" "" code))
           (if mark-active
		       (delete-region (region-beginning) (region-end))
	         (when (> length 0)
		       (delete-char (- 0 length))))
           (when (> length 0)
	         (setq unread-command-events
                   (append (listify-key-sequence code)
                           unread-command-events))))
          (t (message "`+rime-convert-string-at-point' did nothing.")))))


;;;; provide
(provide 'lib-rime)
;;; lib-rime.el ends here.
