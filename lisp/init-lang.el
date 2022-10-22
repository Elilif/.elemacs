;; init-lang.el --- Initialize lang.el configurations.	-*- lexical-binding: t -*-

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
(add-hook 'elemacs-first-file-hook #'yas-global-mode)
(with-eval-after-load 'yasnippet
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (add-hook 'minibuffer-setup-hook 'yas-minor-mode)

  ;; Function that tries to autoexpand YaSnippets
  ;; The double quoting is NOT a typo!
  (defun my/yas-try-expanding-auto-snippets ()
    (when (and (memq this-command '(org-self-insert-command
                                    self-insert-command))
               (boundp 'yas-minor-mode) yas-minor-mode)
      (let* ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand))))
  
  ;; `yas--template-can-expand-p' should return `nil' when `CONDITION'
  ;; and `REQUIREMENT' are both `nil'.
  (defun yas--template-can-expand-p (condition requirement)
    "Evaluate CONDITION and REQUIREMENT and return a boolean."
    (let* ((result (or (null condition)
                       (yas--eval-condition condition))))
      (cond ((eq requirement t)
             result)
            ((and (null requirement)
                  (null result))
             nil)
            (t
             (eq requirement result)))))

  ;; Try after every insertion
  (add-hook 'post-command-hook #'my/yas-try-expanding-auto-snippets)
  (defun eli/latex-smart-kill ()
    "Kill equations, numbers or something else before point in latex math mode.

This function is dedicated for auto yasnippet expanding, for
instance: \"$4\pi^2 //$\" will be expand into
\"\\frac{4\pi^2}{*}\", and this function must be used with
`eli/latex-smart-paste'."
    (condition-case nil
        (save-excursion
          (let* ((orig-point (point))
                 (pre-sexp-point (progn
                                   (backward-sexp)
                                   (point)))
                 (bol (line-beginning-position))
                 (bound-before-target (re-search-backward "\s\\|\\\\(\\|\\$" bol t)))
            (cond
             ((= (1- pre-sexp-point) bound-before-target)
              (kill-region pre-sexp-point orig-point))
             ((null bound-before-target)
              (kill-region bol orig-point))
             ((member (match-string 0) '(" " "$"))
              (kill-region (1+ bound-before-target) orig-point))
             ((string= (match-string 0) "\\(")
              (kill-region (+ bound-before-target 2) orig-point)))))
      (error (setq numerator 'nil))))

  (defun eli/latex-smart-paste ()
    "Paste text killed by `eli/latex-smart-kill'."
    (if numerator
        (let ((temp (string-clean-whitespace (current-kill 0))))
          (if (string-match "^(\\(.*\\))$" temp)
              (match-string 1 temp)
            temp)))))

(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (add-hook 'vterm-copy-mode-hook (lambda () (call-interactively 'hl-line-mode))))

;; electric operator 
(add-hook 'c++-mode-hook #'electric-operator-mode)

(with-eval-after-load 'org-mode
  (defun eli/filter-electric-operator-get-rules-list (list)
    "Enable `electric-operator-mode' in math environments of org-mode.

This function is a advice for `electric-operator-get-rules-list',
whose result is LIST."
    (if (and (eq major-mode 'org-mode)
             (texmathp))
        (if (electric-operator--latex-in-math?)
            (electric-operator-get-rules-trie-for-mode 'latex-math)
          (if electric-operator-enable-in-docs
              (electric-operator-get-rules-trie-for-mode 'text-mode)
            (make-electric-operator--trie)))
      list))
  (advice-add 'electric-operator-get-rules-list :filter-return
              #'eli/filter-electric-operator-get-rules-list))

(provide 'init-lang)
;;; init-lang.el ends here.
