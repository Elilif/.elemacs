;; lib-yasnippet.el --- Initialize lib-yasnippet configurations.	-*- lexical-binding: t; -*-

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
(defvar eli/latex-smart-numerator t)
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
    (error (setq eli/latex-smart-numerator 'nil))))

(defun eli/latex-smart-paste ()
  "Paste text killed by `eli/latex-smart-kill'."
  (if eli/latex-smart-numerator
      (let ((temp (string-clean-whitespace (current-kill 0))))
        (if (string-match "^(\\(.*\\))$" temp)
            (match-string 1 temp)
          temp))))

;; C/C++ mode
(defun eli/c-fun-has-namespace-p (namespace)
  "Predicate whether the current function has NAMESPACE namespace."
  (save-excursion
    (c-beginning-of-defun)
    (unless (re-search-forward
             (concat "^\s*using\\s-+namespace "
                     namespace
                     ";")
             (save-excursion
               (c-end-of-defun)
               (point)) 'no-errer)
      (concat namespace "::"))))

(defun eli/tex-algorithm-p ()
  (when (functionp 'xenops-math-parse-algorithm-at-point)
    (xenops-math-parse-algorithm-at-point)))


;;;; provide
(provide 'lib-yasnippet)
;;; lib-yasnippet.el ends here.
