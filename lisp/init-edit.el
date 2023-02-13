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
  (require 'expand-region-core)
  (require 'er-basic-expansions)
  (require 'lib-expand-region)
  (require 'leetcode)
  (require 'cc-cmds))

;;;; supser save
(setup super-save
  (:once (list :hooks 'find-file-hook)
    (:once (list :hooks 'post-self-insert-hook)
	  (super-save-mode)))
  (:option* super-save-triggers '(magit-status
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
                              (texmathp))))))))

;;;; expand-region
(setup expand-region
  (:once (list :before 'er/expand-region)
	(require 'lib-expand-region))
  (:global "C-=" er/expand-region)
  (:when-loaded
    (setq-default er/try-expand-list '(er/mark-word
                                       er/mark-symbol
                                       er/mark-symbol-with-prefix
                                       er/mark-next-accessor
                                       er/mark-method-call
                                       er/mark-inside-quotes
                                       er/mark-outside-quotes
                                       er/mark-inside-pairs
                                       er/mark-outside-pairs
                                       er/mark-comment
                                       er/mark-block-comment
                                       er/mark-url
                                       er/mark-email
                                       er/mark-defun
                                       er/mark-sentence
                                       mark-paragraph
                                       )))
  (:advice er/expand-region :before eli/er-clearn-history))


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
   kmacro-keyboard-quit :after eli/speed-up-kmacro-recover
   markmacro-exit :after eli/speed-up-kmacro-recover)
  (:global
   "s-=" markmacro-apply-all
   "s--" kmacro-start-macro
   "s-w" markmacro-mark-words
   "s-l" markmacro-mark-lines
   "C-r" rectangle-mark-mode
   "s-g" markmacro-secondary-region-set
   "s-a" markmacro-secondary-region-mark-cursors
   "s-s" markmacro-swap-region
   "s-f" markmacro-mark-current-or-next-target
   "s-b" markmacro-mark-current-or-previous-target
   "s-u" markmacro-unmark-current-target))
;;;; smartparens
(setup smartparens
  (:when-loaded
	(require 'lib-smartparens))
  (:once (list :hooks 'find-file-hook 'prog-mode-hook 'org-mode-hook)
    (:once (list :hooks 'pre-command-hook)
	  (smartparens-global-mode)))
  (:also-load smartparens-config)
  (:when-loaded
    (sp-pair "（" "）")
    (sp-pair "《" "》")
    (sp-pair "“" "”"))
  (:after smartparens-org
    (sp-with-modes 'org-mode
	  (sp-local-pair "/" "/" :unless '(sp-org-point-after-left-square-bracket-p sp-in-math-p sp-in-algorithm-p) :post-handlers '(("[d1]" "SPC")))
	  (sp-local-pair "*" "*" ;;sp-point-after-word-p
					 :unless '(sp-point-at-bol-p sp-in-math-p sp-in-algorithm-p)
					 :post-handlers '(("[d1]" "SPC"))
					 :skip-match 'sp--org-skip-asterisk)
	  (sp-local-pair "=" "=" :unless '(sp-in-algorithm-p sp-in-math-p) :post-handlers '(("[d1]" "SPC")))
	  (sp-local-pair "_" "_" :unless '(sp-in-algorithm-p sp-in-math-p) :post-handlers '(("[d1]" "SPC")))
	  (sp-local-pair "~" "~" :unless '(sp-in-algorithm-p sp-in-math-p) :post-handlers '(("[d1]" "SPC")))))  
  (:advice show-paren-function :around fix-show-paren-function))


;;;; yasnippet
(setup yasnippet
  (:once (list :hooks 'find-file-hook)
    (:once (list :hooks 'post-self-insert-hook)
      (:silence (yas-global-mode))
	  (require 'lib-yasnippet)
	  (:hooks post-command-hook my/yas-try-expanding-auto-snippets)))  
  (:when-loaded
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
    (:option yas-triggers-in-field t)))

;;;; electric-operator
(setup electric-operator
  (:hook-into c++-mode)
  (:hook-into c-mode)
  (:hook-into org-mode))

;;;; aggressive-indent
(setup aggressive-indent
  (:hook-into emacs-lisp-mode-hook)
  (:hook-into c-mode-common-hook)
  (:when-loaded
    (add-to-list
     'aggressive-indent-dont-indent-if
     '(and (or (derived-mode-p 'c++-mode)
               (derived-mode-p 'c-mode))
           (null (string-match "\\([;{}]\\|\\b*\\(if\\|for\\|while\\|return\\)\\b\\)"
                               (thing-at-point 'line)))))))

;;;; misc
(setup prog-mode
  (:hook display-fill-column-indicator-mode
         rainbow-delimiters-mode))

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
