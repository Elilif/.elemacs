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
(elemacs-require-package 'yasnippet)
(add-hook 'elemacs-first-file-hook #'yas-global-mode)
(with-eval-after-load 'yasnippet
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (add-hook 'minibuffer-setup-hook 'yas-minor-mode)

  ;; Function that tries to autoexpand YaSnippets
  ;; The double quoting is NOT a typo!
  (defun my/yas-try-expanding-auto-snippets ()
    (when (bound-and-true-p yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
	(yas-expand))))

  ;; Try after every insertion
  (add-hook 'post-command-hook #'my/yas-try-expanding-auto-snippets)
  (defun eli-latex-smart-kill ()
    (condition-case nil
        (save-excursion
          (let ((ip (point))
                (void (backward-sexp))
                (cp (point))
                (beg-of-line (save-excursion (beginning-of-line) (point)))
                (bp (re-search-backward "\s")))
            (if (= (1- cp) bp)
                (kill-region cp ip)
              (if (< bp beg-of-line)
                  (if (= cp beg-of-line)
                      (kill-region cp ip)
                    (kill-region beg-of-line ip))
                (kill-region (1+ bp) ip)))))
      (error (setq numerator 'nil))))

  (defun eli-latex-smart-paste ()
    (if numerator
        (let ((temp (string-clean-whitespace (current-kill 0))))
          (if (string-match "^(\\(.*\\))$" temp)
              (match-string 1 temp)
            temp))))
  )

(elemacs-require-package 'auto-yasnippet)

(elemacs-require-package 'quickrun)

(elemacs-require-package 'vterm)

(provide 'init-lang)
;;; init-lang.el ends here.
