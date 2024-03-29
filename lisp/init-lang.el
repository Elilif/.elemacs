;; init-lang.el --- Initialize lang configurations.     -*- lexical-binding: t -*-

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
  (require 'cc-mode)
  (require 'lib-lsp)
  (require 'leetcode)
  (require 'lsp-mode)
  (require 'lib-leetcode)
  (require 'lib-vterm)
  (require 'lib-gdb))



;;;; vterm
(setup vterm
  (:iload vterm)
  (:also-load
   lib-vterm)
  (:hook (lambda () (setq-local global-hl-line-mode nil)))
  (:with-hook vterm-copy-mode-hook
    (:hook (lambda () (call-interactively 'hl-line-mode))))
  (:global
   "s-o" shell-pop-posframe-toggle)
  (:bind
   "s-o" eli/vterm-quit))


;;;; leetcode
(setup leetcode
  (:iload leetcode)
  (:when-loaded
    (require 'lib-leetcode))
  (:option*
   leetcode-prefer-language "cpp"
   leetcode-save-solutions t)
  (:bind-into leetcode--problems-mode-map
    "Q" leetcode-quit
    "D" leetcode-daily)
  (:advice leetcode--buffer-content :override eli/leetcode--buffer-content))

;;;; c/c++
(setup cc-mode
  (:iload ccls)
  (:option* c-default-style '((java-mode . "java")
                              (awk-mode . "awk")
                              (c-mode . "gnu")
                              (c++-mode . "stroustrup")
                              (other . "gnu")))
  (:also-load
   ccls)
  (:hooks c-mode-common-hook eli/compile-set
          c++-mode-hook modern-c++-font-lock-mode)
  (:bind-into c-mode-base-map
    "(" nil
    "{" nil
    "C-c C-o" ff-find-other-file)
  (:when-loaded
    (add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-mode))))

(setup ccls
  (:option*
   ccls-enable-skipped-ranges nil))


(defun eli/compile-set ()
  (let* ((file-name (buffer-file-name))
         (is-windows (equal 'windows-nt system-type))
         (exec-suffix (if is-windows ".exe" ".out")))
    (when file-name
      (setq file-name (file-name-nondirectory file-name))
      (let ((out-file (concat (file-name-sans-extension file-name) exec-suffix)))
        (setq-local compile-command (pcase major-mode
                                      ('c++-mode
                                       (format "g++ -std=c++11 -g %s -o %s"
                                               file-name out-file))
                                      ('c-mode
                                       (format "gcc -std=c89 -g %s -o %s"
                                               file-name out-file))))))))
;;;; gdb
(setup gdb-mi
  (:also-load
   lib-gdb)
  (:option*
   gdb-show-main t
   gdb-many-windows t
   gdb-debuginfod-enable nil
   gdb-debuginfod-enable-setting nil)
  (:advice
   gud-query-cmdline :before eli/reset-gud-gdb-history))

;;;; r
(setup ess
  (:option*
   ess-use-flymake nil
   ess-ask-for-ess-directory nil
   ess-history-file "~/.R/.history"
   ess-tab-complete-in-script t
   comint-prompt-read-only t
   ess-use-eldoc 'script-only
   ess-use-company 'script-only))

(setup ess-smart-equals
  (:after ess
    (ess-smart-equals-activate))
  (:option*
   ess-smart-equals-extra-ops '(brace paren percent)))
;;;; lisp
(setup racket-mode
  (:hook
   racket-xp-mode))

(setup ob-racket
  (:option*
   org-babel-default-header-args:racket '((:session . "none")
                                          (:results . "output"))
   ob-racket-custom-code-templates `((value-program . (prolog
                                                       "\n" "(write (let ()"
                                                       "\n" define-vars
                                                       "\n" :body "))"
                                                       "\n" epilogue))
                                     (output-program . (prolog
                                                        "\n" define-vars
                                                        "\n" :body
                                                        "\n" epilogue))
                                     (prolog . (lang-line
                                                "\n" requires
                                                "\n" prologue))
                                     (prologue . nil)
                                     (requires . ,(lambda (_env params)
                                                    (let* ((req (assq :require params))
                                                           (lang (or (cdr (assq :lang params))
                                                                     ob-racket-default-lang))
                                                           (req-string (if (string= lang "racket")
                                                                           "require"
                                                                         "#%require")))
                                                      (when req
                                                        `(parens (spaced ,req-string ,(cdr req))))))))))
;;;; python
(setup lsp-pyright
  (:iload lsp-pyright))

;;;; Web
(setup web-mode
  (:init
   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
  (:option*
   web-mode-enable-auto-quoting nil
   web-mode-enable-auto-expanding t
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2))

(setup impatient-mode
  (:hook-into
   web-mode-hook
   js2-mode
   css-mode)
  (:hook
   httpd-start))

(setup css-mode
  (:option*
   css-indent-offset 2
   css-fontify-colors nil))

(setup js
  (:option*
   js-indent-level 2))

(setup js2-mode
  (:init
   (add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))))

;;;; lsp
(setup lsp-mode
  (:iload lsp-mode)
  (:when-loaded
    (require 'lib-lsp)
    (setenv "LSP_USE_PLISTS" "true")
    (keymap-unset lsp-mode-map "s-l" lsp-command-map)

    ;; (defconst ccls-args nil)
    ;; (defconst clangd-args '("-j=2"
    ;;                         "--malloc-trim"
    ;;                         "--background-index"
    ;;                         "--clang-tidy"
    ;;                         "--completion-style=bundled"
    ;;                         "--pch-storage=disk"
    ;;                         "--header-insertion=iwyu"
    ;;                         "--header-insertion-decorators"
    ;;                         "--include-cleaner-stdlib"))
    ;; (cond ((executable-find "clangd") (setq lsp-clients-clangd-executable "clangd"
    ;;                                         lsp-clients-clangd-args clangd-args))
    ;;       ((executable-find "ccls") (setq lsp-clients-clangd-executable "ccls"
    ;;                                       lsp-clients-clangd-args ccls-args)))
    )
  (:option*
   lsp-session-file "/home/eli/.emacs.d/var/.lsp-session-v1"
   lsp-warn-no-matched-clients nil
   ;; lsp-disabled-clients '(ccls)
   lsp-keymap-prefix nil
   read-process-output-max (* 1024 1024)
   lsp-completion-provider :none
   lsp-keep-workspace-alive nil
   lsp-signature-auto-activate nil
   lsp-modeline-code-actions-enable nil
   lsp-modeline-diagnostics-enable nil
   lsp-modeline-workspace-status-enable nil
   lsp-headerline-breadcrumb-enable t

   lsp-enable-file-watchers nil
   lsp-enable-folding nil
   lsp-enable-symbol-highlighting nil
   lsp-enable-text-document-color t

   lsp-enable-indentation nil
   lsp-enable-on-type-formatting nil
   lsp-lens-enable nil
   lsp-idle-delay 0.5
   lsp-auto-touch-files nil)
  (:hooks prog-mode-hook (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode
                                                   'snippet-mode)
                             (lsp-deferred)))
          ;; lsp-mode-hook (lambda ()
          ;;                  ;; Integrate `which-key'
          ;;                  ;; (lsp-enable-which-key-integration)
          ;;                  ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
          ;;                  ;; (add-hook 'before-save-hook #'lsp-organize-imports t t)
          ;;                  )
          lsp-completion-mode-hook my/lsp-mode-setup-completion))

(setup lsp-ui
  (:iload lsp-ui)
  (:hook-into lsp-mode)
  (:option*
   lsp-ui-sideline-show-hover t
   lsp-ui-sideline-ignore-duplicate t
   lsp-ui-doc-enable nil
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-show-diagnostics t
   lsp-ui-sideline-show-code-actions t
   lsp-ui-doc-show-with-cursor t)
  (:advice
   lsp-ui-doc--hide-frame :around eli/lsp-ui-doc--hide-frame))
;;;; flycheck
(setup flycheck
  (:option*
   flycheck-idle-change-delay 0.5))
;;;; eglot
;; (setup eglot
;;   (:option* eglot-confirm-server-initiated-edits nil
;;              eglot-autoreconnect 60
;;              eglot-autoshutdown t)
;;   (:hooks c++-mode-hook eglot-ensure)
;;   (:when-loaded
;;      (custom-set-faces
;;       '(eglot-mode-line ((t (:foreground "#B0BEC5")))))
;;      (add-to-list 'eglot-server-programs '(c++-mode . ("ccls")))
;;     (add-to-list 'eglot-server-programs '(c-mode . ("ccls")))))
;;;; flymake
;; (setup flymake
;;   (:option*
;;    flymake-no-changes-timeout 0.5))

(setup zeal-at-point
  (:global
   "C-h y" zeal-at-point))

;;;; provide
(provide 'init-lang)
;;; init-lang.el ends here.
