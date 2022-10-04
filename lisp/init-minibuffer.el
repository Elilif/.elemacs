;; init-minibuffer.el --- Initialize minibuffer configurations.	-*- lexical-binding: t -*-

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

(add-hook 'elemacs-first-input-hook 'vertico-mode)
(with-eval-after-load 'vertico
  (setq vertico-cycle t))

(with-eval-after-load 'vertico
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

;; embark
(with-eval-after-load 'vertico
  (keymap-global-set "C-." #'embark-act)
  (keymap-global-set "M-." #'embark-dwim)
  (keymap-global-set "C-h B" #'embark-bindings)

  (setq prefix-help-command #'embark-prefix-help-command)

  ;; preview image while using `find-file'
  (defun eli-image-preview (&rest _args)
    (let* ((target (embark--targets))
           (file-path (plist-get (car target) :target))
           (name (file-name-nondirectory file-path))
           (mode (assoc-default name auto-mode-alist #'string-match)))
      (posframe-hide-all)
      (when (memq mode '(image-mode))
        (with-current-buffer (get-buffer-create "*image*")
          (setq inhibit-read-only t)
          (erase-buffer)
          (insert-file-contents file-path)
          (set-auto-mode-0 mode))
        (when (posframe-workable-p)
          (posframe-show "*image*"
                         :poshandler #'posframe-poshandler-frame-center)))))

  (defun eli-select-images ()
    (interactive)
    (let ((default-directory "~/Documents/org-images/"))
      (call-interactively 'find-file)))
  
  (advice-add 'eli-select-images
              :before (lambda (&rest _args)
                        (add-hook 'post-command-hook #'eli-image-preview)))

  (add-hook 'minibuffer-exit-hook
            (lambda ()
              (remove-hook 'post-command-hook #'eli-image-preview)
              (posframe-delete-all))))

(with-eval-after-load 'embark
  (require 'embark-consult))

(with-eval-after-load 'vertico
  (require 'orderless)
  (with-eval-after-load 'pinyinlib
    (defun completion--regex-pinyin (str)
      (orderless-regexp (pinyinlib-build-regexp-string str)))
    (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))
  ;; https://github.com/minad/consult/wiki
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))
  (defun +orderless--suffix-regexp ()
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--suffix-regexp))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--suffix-regexp))))
     ;; Ignore single !
     ((equal "!" word) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref word 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring word 1))
        (when-let (x (assq (aref word (1- (length word))) +orderless-dispatch-alist))
          (cons (cdr x) (substring word 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides
        '((buffer (styles basic partial-completion))
          (file (styles basic partial-completion))
          (command (styles +orderless-with-initialism))
          (variable (styles +orderless-with-initialism))
          (symbol (styles +orderless-with-initialism)))
        ;; allow escaping space with backslash!
        orderless-component-separator #'orderless-escapable-split-on-space
        orderless-style-dispatchers '(+orderless-dispatch)))

(with-eval-after-load 'vertico
  (setq marginalia-align 'right)
  (marginalia-mode))

(keymap-global-set "C-s" #'consult-line)
(keymap-global-set "C-x C-r" #'consult-recent-file)
(keymap-global-set "C-c \\" #'consult-register)
(keymap-global-set "C-c -" #'consult-register-load)
(keymap-global-set "C-c =" #'consult-register-store)
(keymap-global-set "C-x b" #'consult-buffer)
(keymap-global-set "M-g o" #'consult-outline)
(with-eval-after-load 'vertico
  (require 'consult)
  
  (defun my/consult-org-file (&optional match)
    (interactive)
    (consult-org-heading match '(list org-agenda-file-inbox org-agenda-file-habit org-agenda-file-projects)))
  
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep my/consult-org-file
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-.")))

(add-hook 'minibuffer-setup-hook 'savehist-mode)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here.
