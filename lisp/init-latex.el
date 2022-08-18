;; init-latex.el --- Initialize latex configurations.	-*- lexical-binding: t -*-

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

(elemacs-require-package 'auctex)
(with-eval-after-load 'tex
  (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)
  (add-hook 'LaTeX-mode-hook #'tex-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook (lambda () (setq TeX-command-default "XeLaTeX")))
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (setq-default TeX-engine 'xetex)
  (setq LaTeX-command "xelatex")
  (setq TeX-source-correlate-start-server t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq LaTeX-biblatex-use-Biber t)
  (setq LaTeX-item-indent 0)
  (setq TeX-show-compilation nil)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	    TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))

  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --shell-escape --synctex=1%(mode)%' %t" TeX-run-TeX nil t))

  ;; reftex
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography eli/bibliography)

  ;; preview
  )

(elemacs-require-package 'lsp-latex)
(with-eval-after-load 'tex
  (add-hook 'LaTeX-mode-hook #'lsp-deferred)
  (setq lsp-latex-texlab-executable "/usr/bin/texlab")
  (setq lsp-tex-server 'texlab)
  )


;; (elemacs-require-package 'cdlatex)
;; (with-eval-after-load 'tex
;;   (keymap-set cdlatex-mode-map "$" nil)
;;   (keymap-set cdlatex-mode-map "\(" nil)
;;   (add-hook 'LaTeX-mode #'turn-on-cdlatex)
;;   (add-hook 'cdlatex-tab #'cdlatex-in-yas-field)

;;   (defun cdlatex-in-yas-field ()
;;     ;; Check if we're at the end of the Yas field
;;     (when-let* ((_ (overlayp yas--active-field-overlay))
;;                 (end (overlay-end yas--active-field-overlay)))
;;       (if (>= (point) end)
;;           ;; Call yas-next-field if cdlatex can't expand here
;;           (let ((s (thing-at-point 'sexp)))
;;             (unless (and s (assoc (substring-no-properties s)
;;                                   cdlatex-command-alist-comb))
;;               (yas-next-field-or-maybe-expand)
;;               t))
;;         ;; otherwise expand and jump to the correct location
;;         (let (cdlatex-tab-hook minp)
;;           (setq minp
;;                 (min (save-excursion (cdlatex-tab)
;;                                      (point))
;;                      (overlay-end yas--active-field-overlay)))
;;           (goto-char minp) t))))

;;   (defun yas-next-field-or-cdlatex ()
;;     (interactive)
;;     "Jump to the next Yas field correctly with cdlatex active."
;;     (if (bound-and-true-p cdlatex-mode)
;;         (cdlatex-tab)
;;       (yas-next-field-or-maybe-expand)))
;;   (setq cdlatex-paired-parens "$([{|<")
;;   )
;; ;; yasnippet support
;; (with-eval-after-load 'tex
;;   (add-hook 'cdlatex-tab #'yas-expand)
;;   (keymap-set cdlatex-mode-map "<tab>" #'cdlatex-tab)
;;   (keymap-set yas-keymap "<tab>" #'yas-next-field-or-cdlatex)
;;   (keymap-set yas-keymap "TAB" #'yas-next-field-or-cdlatex)
;; )

;; (elemacs-require-package 'xenops)
;; (with-eval-after-load 'tex
;;   (add-hook 'LaTeX-mode #'xenops-mode)
;;   (setq xenops-math-image-scale-factor 1.3)
;;   (setq xenops-image-try-write-clipboard-image-to-file nil))

(elemacs-require-package 'org-latex-impatient)
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'org-latex-impatient-mode)
  (setq org-latex-impatient-tex2svg-bin
        ;; location of tex2svg executable
        "~/node_modules/mathjax-node-cli/bin/tex2svg")
  (setq org-latex-impatient-posframe-position 'point))

(provide 'init-latex)
;;; init-latex.el ends here.
