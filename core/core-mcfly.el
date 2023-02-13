;; core-mcfly.el --- Initialize core-mcfly.el.	-*- lexical-binding: t; -*-

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

(defgroup elemacs-mcfly nil
  "Elemacs fly key."
  :group 'elemacs)

(defcustom elemacs-mcfly-commands '(consult-line
                                    consult-outline
                                    consult-git-grep
									eli/consult-git-grep
                                    consult-ripgrep
                                    my-search-with-chrome)
  "Commands that need pre-insert candidate."
  :group 'elemacs-mcfly
  :type '(repeat function))

(defvar elemacs-mcfly-back-commands '(self-insert-command
                                      yank
                                      yank-pop
                                      org-yank))

(defun elemacs-mcfly-back-to-present ()
  (remove-hook 'pre-command-hook 'elemacs-mcfly-back-to-present t)
  (cond ((and (memq last-command elemacs-mcfly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command elemacs-mcfly-back-commands)
         (delete-region (point) (point-max)))))

(defun elemacs-mcfly-time-travel ()
  (when (memq this-command elemacs-mcfly-commands)
    (let ((pre-insert-string (with-minibuffer-selected-window
                               (or (seq-some
                                    (lambda (thing) (thing-at-point thing t))
					                '(region url symbol))
					               ;; '(symbol url region sexp))
			                       "No thing at point"))))
      (save-excursion
        (insert (propertize pre-insert-string 'face 'shadow))))
    (add-hook 'pre-command-hook 'elemacs-mcfly-back-to-present nil t)))

;; setup code
(add-hook 'minibuffer-setup-hook #'elemacs-mcfly-time-travel)



(provide 'core-mcfly)
;;; core-mcfly.el ends here.
