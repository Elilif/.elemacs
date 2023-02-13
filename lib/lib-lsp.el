;; lib-lsp.el --- Initialize lib-lsp configurations.	-*- lexical-binding: t; -*-

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

(defun my/lsp-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))

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
  '("python" "ipython" "c" "cpp" "c++" "shell"))
;; (add-to-list 'org-babel-lang-list (if emacs/>=26p "shell" "sh"))
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))


;;;; provide
(provide 'lib-lsp)
;;; lib-lsp.el ends here.
