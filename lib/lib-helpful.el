;; lib-helpful.el --- Initialize lib-helpful configurations.	-*- lexical-binding: t; -*-

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

(defun +helpful-switch-to-buffer (buffer-or-name)
  "Switch to helpful BUFFER-OR-NAME. The logic is simple, if we are
currently in the helpful buffer, reuse it's window, otherwise
create new one."
  (if (eq major-mode 'helpful-mode)
	  (switch-to-buffer buffer-or-name)
    (pop-to-buffer buffer-or-name)))

(defun helpful-set-arguments-face (&rest _args)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (limit (save-excursion
                   (re-search-forward "^Source Code\n" nil t))))
      (while (re-search-forward
              "\\<[-A-Z]+\\>" limit t)
        (overlay-put (make-overlay
                      (match-beginning 0) (match-end 0))
                     'face 'help-argument-name)))))


;;;; provide
(provide 'lib-helpful)
;;; lib-helpful.el ends here.
