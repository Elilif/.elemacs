;; init-completion.el --- Initialize completion configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 by Eli

;; Author: Eli <eli.q.qian@gmail.com>
;; URL: https://github.com/Elilif/.emacs.d

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

(add-hook 'elemacs-first-file-hook (lambda ()
					(require 'smartparens-config)
					(smartparens-global-mode)))
(with-eval-after-load 'smartparens
  (sp-pair "（" "）")
  (sp-pair "《" "》")
  (sp-pair "“" "”")
  ;; improving emphasis marker
  (with-eval-after-load 'smartparens-org
    (sp-with-modes 'org-mode
      (sp-local-pair "/" "/" :unless '(sp-org-point-after-left-square-bracket-p sp-in-math-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "*" "*" ;;sp-point-after-word-p
                     :unless '(sp-point-at-bol-p sp-in-math-p)
                     :post-handlers '(("SPC"))
                     :skip-match 'sp--org-skip-asterisk)
      (sp-local-pair "=" "=" :unless '(sp-in-math-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "_" "_" :unless '(sp-in-math-p))
      (sp-local-pair "~" "~" :unless '(sp-in-math-p) :post-handlers '(("[d1]" "SPC")))))
  
  (define-advice show-paren-function (:around (fn) fix-show-paren-function)
    "Highlight enclosing parens."
    (cond ((looking-at-p "\\s(") (funcall fn))
	  (t (save-excursion
	       (ignore-errors (backward-up-list))
	       (funcall fn))))))

(defvar mcfly-commands
  '(consult-line
    consult-outline
    consult-git-grep
    consult-ripgrep
    my-search-with-chrome))

(defvar mcfly-back-commands
  '(self-insert-command
    yank
    yank-pop
    org-yank))

(defun mcfly-back-to-present ()
  (remove-hook 'pre-command-hook 'mcfly-back-to-present t)
  (cond ((and (memq last-command mcfly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command mcfly-back-commands)
         (delete-region
	  (progn (forward-visible-line 0) (point))
          (point-max)))))

(defun mcfly-time-travel ()
  (when (memq this-command mcfly-commands)
    (insert (propertize (save-excursion
			  (set-buffer (window-buffer (minibuffer-selected-window)))
			  (or (seq-some (lambda (thing) (thing-at-point thing t))
					;; '(region url symbol sexp))
					'(url symbol))
			      "No thing at point")
			  )    'face 'shadow))
    (add-hook 'pre-command-hook 'mcfly-back-to-present nil t)
    (forward-visible-line 0)))

;; setup code
(add-hook 'minibuffer-setup-hook #'mcfly-time-travel)

(provide 'init-completion)
;;; init-completion.el ends here.
