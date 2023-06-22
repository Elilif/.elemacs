;; init-edit.el --- Initialize edit configurations.	-*- lexical-binding: t -*-

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

(cl-eval-when (compile)
  (require 'org-fold-core)
  (require 'leetcode)
  (require 'electric-operator)
  (require 'cc-cmds)
  (require 'lib-markmacro))

;;;; supser save
(setup super-save
  (:once (list :hooks 'find-file-hook)
    (:once (list :hooks 'post-self-insert-hook)
	  (super-save-mode)))
  (:option*
   super-save-auto-save-when-idle t
   super-save-triggers '(magit-status
                         ace-window
                         switch-to-buffer
                         other-window
                         windmove-up
                         windmove-down
                         windmove-left
                         windmove-right
                         next-buffer
                         previous-buffer)
   super-save-hook-triggers '(find-file-hook
                              mouse-leave-buffer-hook
                              focus-out-hook)
   super-save-remote-files nil
   super-save-predicates
   '((lambda () buffer-file-name)
     (lambda () (buffer-modified-p (current-buffer)))
	 (lambda () (and (boundp 'org-noter-notes-mode)
					 (not org-noter-notes-mode)))
     (lambda () (file-writable-p buffer-file-name))
     (lambda () (if (and super-save-max-buffer-size (> super-save-max-buffer-size 0))
                    (< (buffer-size) super-save-max-buffer-size)
                  t))
     (lambda ()
       (if (file-remote-p buffer-file-name) super-save-remote-files t))
     (lambda () (super-save-include-p buffer-file-name))
     (lambda ()
       (not (or (and (functionp 'xenops-math-parse-algorithm-at-point)
                     (xenops-math-parse-algorithm-at-point))
                (and (functionp 'texmathp)
                     (texmathp)))))))
  
  (:advice super-save-command :override (lambda () (let ((inhibit-message t))
													 (save-some-buffers t)))
		   save-buffer :around suppress-messages))

;;;; mwim
(setup mwim
  (:global [remap move-beginning-of-line] mwim-beginning
           [remap move-end-of-line] mwim-end))

;;;; hungry-delete
(setup hungry-delete
  (once (list :before 'backward-delete-char-untabify 'delete-backward-char
			  :hooks 'post-self-insert-hook)
    (global-hungry-delete-mode)))

;;;; markmacro
(setup markmacro
  (:also-load lib-markmacro)
  (:advice
   kmacro-start-macro :before eli/speed-up-kmacro
   markmacro-exit :after eli/speed-up-kmacro-recover)
  (:global
   "s-`" markmacro-apply-all
   "s-\\" kmacro-start-macro
   "s-w" markmacro-mark-words
   "s-l" markmacro-mark-lines
   "C-r" rectangle-mark-mode
   "s-g" markmacro-secondary-region-set
   "s-a" markmacro-secondary-region-mark-cursors
   "s-s" markmacro-swap-region
   "s-f" markmacro-mark-current-or-next-target
   "s-b" markmacro-mark-current-or-previous-target
   "s-u" markmacro-unmark-current-target))
;;;; electric pair
(setup elec-pair
  (:once (list :before 'self-insert-command)
	(electric-pair-mode))
  (:also-load
   lib-elec-pair)
  (:option*
   electric-pair-inhibit-predicate #'eli/electric-pair-inhibit
   electric-pair-pairs `(,electric-pair-pairs
						 ("《" . "》")
						 ("（" . "）")))
  (:after org
	(modify-syntax-entry ?/ "(/" org-mode-syntax-table)
	(modify-syntax-entry ?* "(*" org-mode-syntax-table)
	(modify-syntax-entry ?= "(=" org-mode-syntax-table)
	(modify-syntax-entry ?+ "(+" org-mode-syntax-table)
	(modify-syntax-entry ?_ "(_" org-mode-syntax-table)
	(modify-syntax-entry ?~ "(~" org-mode-syntax-table))
  (:hooks
   minibuffer-setup-hook electric-pair-mode))

(setup puni
  (:also-load
   lib-puni)
  (:hook-into prog-mode)
  (:global
   "C-=" eli/expand-region)
  (:bind
   "<DEL>" eli/puni-hungry-backward-delete-char
   "C-M-a" beginning-of-defun
   "C-M-e" end-of-defun
   "C-M-p" puni-beginning-of-sexp
   "C-M-n" puni-end-of-sexp))

;;;; tempel
(setup tempel
  (:once (list :hooks 'pre-command-hook)
	(require 'tempel))
  (:also-load
   lib-tempel)
  (:init
   (setq tempel-path "~/.emacs.d/snippets/tempel/templates"))
  (:when-loaded
	(:hooks
	 prog-mode-hook tempel-setup-capf
	 org-mode-hook tempel-setup-capf))
  (:bind-into tempel-map
	"<tab>" tempel-next
	"C-<tab>" tempel-previous
	"M-a" tempel-beginning
	"M-e" tempel-end
	"C-g" tempel-abort))
;;;; electric-operator
(setup electric-operator
  (:hook-into c++-mode)
  (:hook-into c-mode)
  (:hook-into python-base-mode-hook)
  ;; (:hook-into org-mode)
  (:when-loaded
	(electric-operator-add-rules-for-mode 'c++-mode
										  (cons "&" nil))
	(electric-operator-add-rules-for-mode 'c++-mode
										  (cons "*" nil))))

;;;; aggressive-indent
(setup aggressive-indent
  (:hook-into emacs-lisp-mode-hook)
  (:hook-into c-mode-common-hook)
  (:option*  aggressive-indent-dont-indent-if
			 '((memq (char-before) '(?\t ?\n)) ;; used by hungry-delete
			   (and (or (derived-mode-p 'c++-mode)
						(derived-mode-p 'c-mode))
					(null (string-match "\\([;{}]\\|\\b*\\(if\\|for\\|else\\|while\\|return\\)\\b\\)"
										(thing-at-point 'line)))))))

;;;; misc
(setup prog-mode
  (:hook display-fill-column-indicator-mode
         rainbow-delimiters-mode)
  (:hooks emacs-lisp-mode-hook add-after-save-hook))

;;;; outli
(setup outli
  (:when-loaded
	(require 'lib-outli))
  (:hook-into emacs-lisp-mode-hook)
  (:bind-into outline-minor-mode-map
	"<backtab>" outline-cycle-buffer
	"C-c C-p" outline-previous-visible-heading
	"C-c C-n" outline-next-visible-heading)
  (:option* outli-heading-config '((emacs-lisp-mode ";;;" ?\; t)
								   (tex-mode "%%" ?% t))
			outli-blend nil
			outli-speed-commands
			'(("Outline Navigation")
			  ("n" . outline-next-visible-heading)
			  ("p" . outline-previous-visible-heading)
			  ("f" . outline-forward-same-level)
			  ("b" . outline-backward-same-level)
			  ("u" . outline-up-heading)
			  ("Outline Visibility")
			  ("c" . outline-cycle)
			  ("C" . elemacs/outline-comment-subtree)
			  ("s" . outli-toggle-narrow-to-subtree)
			  ("h" . outline-hide-sublevels)
			  ("1" . (outline-hide-sublevels 1))
			  ("2" . (outline-hide-sublevels 2))
			  ("3" . (outline-hide-sublevels 3))
			  ("4" . (outline-hide-sublevels 4))
			  ("5" . (outline-hide-sublevels 5))
			  ("Outline Structure Editing")
			  ("U" . outline-move-subtree-up)
			  ("D" . outline-move-subtree-down)
			  ("r" . outline-demote)
			  ("l" . outline-promote)
			  ("i" . outli-insert-heading-respect-content)
			  ("@" . outline-mark-subtree) 
			  ("?" . outli-speed-command-help))))


;;;; provide
(provide 'init-edit)
;;; init-edit.el ends here.
