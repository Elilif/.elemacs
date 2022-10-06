;; init-lsp.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

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

(add-hook 'prog-mode-hook (lambda ()
                              (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode)
				(lsp-deferred))))
(add-hook 'lsp-mode-hook (lambda ()
			     ;; Integrate `which-key'
			     (lsp-enable-which-key-integration)))
(with-eval-after-load 'lsp-mode
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure flex
  (add-hook 'lsp-completion-mode-hook #'my/lsp-mode-setup-completion)

  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setenv "LSP_USE_PLISTS" "true")
  
  (setq lsp-completion-provider :none
	lsp-keep-workspace-alive nil
	lsp-signature-auto-activate nil
	lsp-modeline-code-actions-enable nil
	lsp-modeline-diagnostics-enable nil
	lsp-modeline-workspace-status-enable nil
	lsp-headerline-breadcrumb-enable t

	lsp-enable-file-watchers nil
	lsp-enable-folding nil
	lsp-enable-symbol-highlighting t
	lsp-enable-text-document-color t

	lsp-enable-indentation nil
	lsp-enable-on-type-formatting nil
	lsp-lens-enable nil)

  (add-to-list 'lsp-language-id-configuration '(snippet-mode . "plaintext")))

(with-eval-after-load 'org
  (setq eli/selected-lsp 'lsp-mode)
  ;; Enable LSP in org babel
  ;; need to add `:file test.xx' in the header
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (cl-defmacro lsp-org-babel-enable (lang)
    "Support LANG in org source code block."
    (cl-check-type lang string)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
	     (defun ,intern-pre (info)
           (setq buffer-file-name (or (->> info caddr (alist-get :file))
                                      (make-temp-file "babel-lsp-" nil
                                                      (concat
                                                       "."
                                                       (car info)))))
           (pcase eli/selected-lsp
             ('eglot
              (when (fboundp 'eglot-ensure)
		        (eglot-ensure)))
             ('lsp-mode
              (when (fboundp 'lsp-deferred)
		        ;; Avoid headerline conflicts
		        (setq-local lsp-headerline-breadcrumb-enable nil)
		        (lsp-deferred)))
             (_
              (user-error "LSP:: invalid `eli/selected-lsp' type"))))
	     (put ',intern-pre 'function-documentation
              (format "Enable `%s' in the buffer of org source block (%s)."
                      eli/selected-lsp (upcase ,lang)))

	     (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (defvar org-babel-lang-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "c" "rust" "java" "cpp" "c++" "shell"))
  ;; (add-to-list 'org-babel-lang-list (if emacs/>=26p "shell" "sh"))
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enable ,lang))))

(add-hook 'lsp-mode-hook #'lsp-ui-mode)
(with-eval-after-load 'lsp-mode
  (setq lsp-ui-sideline-show-hover t
	    lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable nil
	    lsp-ui-sideline-show-diagnostics t
	    lsp-ui-sideline-show-code-actions t
	    lsp-ui-doc-show-with-cursor t))


(provide 'init-lsp)
;;; init-lsp.el ends here.
