;; init-completion.el --- Initialize completion configurations.	-*- lexical-binding: t -*-

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
  (require 'consult-imenu))


;;;; savehist
(setup savehist
  (:once (list :before 'vertico-mode)
    (savehist-mode t))
  (:option* savehist-additional-variables '((kill-ring . 10)
                                            compile-command)))


;;;; vertico
(setup vertico
  (:once (list :hooks 'pre-command-hook)
    (vertico-mode 1))
  (:option* vertico-cycle t
            vertico-count 8)
  (:bind-into vertico-map
    "RET" vertico-directory-enter
    "DEL" vertico-directory-delete-char
    "M-DEL" vertico-directory-delete-word)
  (:hooks rfn-eshadow-update-overlay-hook vertico-directory-tidy))



;;;; marginlia
(setup marginalia
  (:after vertico
    (marginalia-mode t))
  (:option* marginalia-align 'right))


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
           [remap switch-to-buffer]    eli/consult-buffer
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
     consult--source-bookmark consult--source-recent-file
     consult--source-project-recent-file eli/consult-git-ripgrep
     :preview-key "M-."))
  (:advice consult-ripgrep :around consult--with-orderless
           consult-imenu :around my/consult-imenu-around-advice
           consult-org-heading :before consult--set-previous-point
           ;; consult-outline :before consult--set-previous-point
           vertico--update :after consult-vertico--update-choose))




;;;; orderless
(setup orderless
  (:once (list :packages 'vertico)
	(require 'lib-orderless)
    (require 'orderless))
  (:also-load pinyinlib)
  (:option* completion-styles              '(orderless basic)
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


;;;; corfu config
(setup corfu
  (:once (list :hooks 'prog-mode-hook)
    (global-corfu-mode)
    (:also-load lib-kind-all-the-icons))
  (:option* corfu-cycle       t
	        corfu-auto        t
	        corfu-separator ?\s
	        corfu-max-width 150
	        corfu-auto-prefix 3
            corfu-excluded-modes '(org-mode)
	        corfu-on-exact-match nil
            corfu-margin-formatters '(kind-all-the-icons-margin-formatter)))


;;;; cape
(setup cape
  (:after corfu
    (add-to-list 'completion-at-point-functions #'cape-file)))
;;;; embark
(setup embark
  (:once (list :before 'hydra-bibtex/body)
	(require 'embark)
	(require 'all-the-icons))
  (:also-load lib-embark
			  embark-consult)
  (:global
   "C-." embark-act
   "M-." embark-dwim
   "C-h B" embark-bindings)
  (:option*
   prefix-help-command #'embark-prefix-help-command))

;;;; provide
(provide 'init-completion)
;;; init-completion.el ends here.
