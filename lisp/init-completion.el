;; init-completion.el --- Initialize completion configurations.     -*- lexical-binding: t -*-

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
  (require 'consult-imenu)
  (require 'tabspaces))


;;;; savehist
(setup savehist
  (:once (list :before 'vertico-mode)
    (setq savehist-file "~/.emacs.d/var/history")
    (savehist-mode t))
  (:option*
   savehist-additional-variables '((kill-ring . 10)
                                   compile-command)))


;;;; vertico
(setup vertico
  (:once (list :hooks 'pre-command-hook)
    (vertico-mode 1))
  (:option*
   vertico-cycle t
   vertico-count 8)
  (:bind-into vertico-map
    "RET" vertico-directory-enter
    "DEL" vertico-directory-delete-char
    "M-DEL" vertico-directory-delete-word
    "C-r" vertico-repeat-select)
  (:hooks
   rfn-eshadow-update-overlay-hook vertico-directory-tidy)

  ;; vertico-select
  (:after vertico
    (:hooks
     minibuffer-setup-hook vertico-repeat-save)
    (:after savehist
      (add-to-list 'savehist-additional-variables 'vertico-repeat-history))))



;;;; marginlia
(setup marginalia
  (:after vertico
    (marginalia-mode t)
    (require 'lib-marginalia))
  (:option*
   marginalia-align 'right
   marginalia-prompt-categories '(("\\<customize group\\>" . customize-group)
                                  ("\\<M-x\\>" . command)
                                  ("\\<package\\>" . package)
                                  ("\\<bookmark\\>" . bookmark)
                                  ("\\<color\\>" . color)
                                  ("\\<face\\>" . face)
                                  ("\\<environment variable\\>" . environment-variable)
                                  ("\\<function\\|Callable\\|hook to remove\\>" . function)
                                  ("\\<variable\\>" . variable)
                                  ("\\<input method\\>" . input-method)
                                  ("\\<charset\\>" . charset)
                                  ("\\<coding system\\>" . coding-system)
                                  ("\\<minor mode\\>" . minor-mode)
                                  ("\\<kill-ring\\>" . kill-ring)
                                  ("\\<tab by name\\>" . tab)
                                  ("\\<[Ll]ibrary\\>" . library)))
  (:bind-into minibuffer-local-map
    "M-a" marginalia-cycle)
  (:when-loaded
    (setf (alist-get 'command marginalia-annotator-registry)
          '(eli/marginalia-annotate-command marginalia-annotate-binding builtin none))))


;;;; consult
(setup consult
  (:after vertico
    (require 'lib-consult)
    (require 'consult))
  (:also-load lib-mcfly)
  (:once (list :before 'consult-recent-file)
    (:silence (recentf-mode 1)))
  (:global [remap isearch-forward]     consult-line
           [remap find-file-read-only] consult-recent-file
           [remap list-directory]      consult-dir
           ;; [remap switch-to-buffer]    eli/consult-buffer
           ;; workaround for `tabspaces.el'
           "C-x b"                     eli/consult-buffer
           "C-c \\"                    consult-register
           "C-c -"                     consult-register-store
           "C-c ="                     consult-register-load
           "M-g o"                     consult-outline)
  (:option* consult-fontify-max-size       100000
            xref-show-xrefs-function       #'consult-xref
            xref-show-definitions-function #'consult-xref
            consult-buffer-filter '("\\` "
                                    "\\`\\*Completions\\*\\'"
                                    "\\`\\*Flymake log\\*\\'"
                                    "\\`\\*Semantic SymRef\\*\\'"
                                    "\\`\\*tramp/.*\\*\\'"))
  (:when-loaded
    (consult-customize
     consult-theme
     :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep eli/consult-org-file
     consult-bookmark consult-recent-file consult-xref consult-org-heading
     consult--source-bookmark consult--source-recent-file eli/consult-buffer
     consult--source-project-recent-file eli/consult-git-ripgrep eli/consult-org-roam-heading
     :preview-key "M-."))
  (:advice consult-ripgrep :around consult--with-orderless
           consult-fd :around consult--with-orderless
           consult-imenu :around my/consult-imenu-around-advice
           ;; consult-org-heading :before consult--set-previous-point
           ;; consult-outline :before consult--set-previous-point
           vertico--update :after consult-vertico--update-choose))




;;;; orderless
(setup orderless
  (:once (list :packages 'vertico)
    (require 'lib-orderless)
    (require 'orderless))
  (:also-load pinyinlib)
  (:option* completion-styles              '(orderless prescient)
            completion-category-defaults   nil
            completion-category-overrides  '((buffer (styles basic partial-completion))
                                             (file (styles partial-completion))
                                             (command (styles +orderless-with-initialism))
                                             (variable (styles +orderless-with-initialism))
                                             (symbol (styles +orderless-with-initialism)))
            ;; allow escaping space with backslash!
            orderless-component-separator  #'orderless-escapable-split-on-space
            orderless-style-dispatchers    (list #'+orderless-consult-dispatch
                                                 #'orderless-affix-dispatch)
            orderless-matching-styles      '(completion--regex-pinyin
                                             orderless-literal
                                             orderless-regexp))
  (:when-loaded
    ;; Define orderless style with initialism by default
    (orderless-define-completion-style +orderless-with-initialism
      (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))))

;;; prescient
(setup prescient
  (:autoload prescient-persist-mode)
  (:once (list :packages 'vertico)
    (prescient-persist-mode))
  (:hooks
   corfu-mode-hook corfu-prescient-mode))


;;;; corfu
(setup corfu
  (:also-load
   lib-corfu)
  (:once (list :hooks 'prog-mode-hook 'org-mode-hook)
    (global-corfu-mode))
  (:option* corfu-cycle       t
            corfu-auto        t
            corfu-separator ?\s
            corfu-max-width 150
            corfu-auto-prefix 3
            corfu-excluded-modes nil
            corfu-auto-delay 0.1
            corfu-on-exact-match 'quit
            corfu-preselect nil
            corfu-margin-formatters '(nerd-icons-corfu-formatter))
  (:with-map corfu-map
    (:bind
     "C-n" corfu-complete-common-or-next))
  (:when-loaded
    (:hooks minibuffer-setup-hook corfu-enable-in-minibuffer)))


;;;; cape
(setup cape
  (:once (list :before 'global-corfu-mode)
    (add-hook 'completion-at-point-functions #'cape-file)))

;;;; embark
(setup embark
  (:after consult
    (require 'embark))
  (:once (list :before 'hydra-bibtex/body)
    (require 'all-the-icons))
  (:also-load lib-embark
              embark-consult)
  (:global
   "C-." embark-act
   "M-." embark-dwim
   "C-c ," embark-act-all
   "C-h B" embark-bindings
   "s-d" eli/embark-deselect
   "C-c ;" (kbd "C-. SPC"))
  (:bind-into embark-buffer-map
    "r" tabspaces-remove-selected-buffer
    "R" embark-rename-buffer)
  (:bind-into embark-file-map
    "C" eli/copy-file
    "D" eli/delete-file
    "R" eli/move-file)
  (:bind-into vertico-map
    "C-s" (kbd "C-. SPC"))
  (:when-loaded
    (add-to-list 'embark-post-action-hooks
                 '(eli/move-file eli/move-file-after-action))
    (cl-callf cl-union embark-multitarget-actions eli/multitarget-actions))
  (:option*
   prefix-help-command #'embark-prefix-help-command
   embark-quit-after-action '((t . t)
                              (eli/move-file . nil))
   embark-confirm-act-all nil
   embark-pre-action-hooks '((eval-last-sexp embark--end-of-target)
                             (indent-pp-sexp embark--beginning-of-target)
                             (backward-up-list embark--beginning-of-target)
                             (backward-list embark--beginning-of-target)
                             (forward-list embark--end-of-target)
                             (forward-sexp embark--end-of-target)
                             (backward-sexp embark--beginning-of-target)
                             (raise-sexp embark--beginning-of-target)
                             (kill-sexp embark--beginning-of-target)
                             (mark-sexp embark--beginning-of-target)
                             (transpose-sexps embark--end-of-target)
                             (transpose-sentences embark--end-of-target)
                             (transpose-paragraphs embark--end-of-target)
                             (forward-sentence embark--end-of-target)
                             (backward-sentence embark--beginning-of-target)
                             (backward-paragraph embark--beginning-of-target)
                             (embark-insert embark--end-of-target)
                             (find-library embark--xref-push-marker)
                             (delete-file embark--confirm)
                             (delete-directory embark--confirm)
                             ;; (kill-buffer embark--confirm)
                             ;; (embark-kill-buffer-and-window embark--confirm)
                             (bookmark-delete embark--confirm)
                             (package-delete embark--confirm)
                             (tab-bar-close-tab-by-name embark--confirm)
                             (embark-isearch embark--unmark-target)
                             (occur embark--unmark-target)
                             (query-replace embark--beginning-of-target embark--unmark-target)
                             (query-replace-regexp embark--beginning-of-target embark--unmark-target)
                             (mark embark--mark-target)
                             (shell embark--universal-argument)
                             (eshell embark--universal-argument)
                             (embark-select embark--select))))

;;;; provide
(provide 'init-completion)
;;; init-completion.el ends here.
