;; init-misc.el --- Initialize misc configurations.	-*- lexical-binding: t -*-

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
  (require 'cal-china-x)
  (require 'lib-rime)
  (require 'rime)
  (require 'avy)
  (require 'gptel)
  (require 'gptel-curl)
  (require 'lib-gptel)
  (require 'ledger-mode)
  (require 'lib-ledger-mode)
  (require 'lib-tab-bar))

;;;; dired
(setup dired
  (:once (list :packages 'dired)
    (diredfl-global-mode))
  (:hook all-the-icons-dired-mode)
  (:option* all-the-icons-dired-monochrome nil))

;;;; ibuffer
(setup ibuffer
  (:hook all-the-icons-ibuffer-mode)
  (:option* all-the-icons-ibuffer-icon t
            all-the-icons-ibuffer-color-icon t))


;;;; hl-todo
(setup hl-todo
  (:once (list :hooks 'find-file-hook)
    (global-hl-todo-mode)))

;;;; popper
(setup popper
  (:once (list :hooks 'find-file-hook)
    (popper-mode)
    (popper-echo-mode))
  (:autoload popper-toggle-latest)
  (:autoload popper-cycle)
  (:autoload popper-toggle-type)
  (:global "C-`" #'popper-toggle-latest
           "M-`" #'popper-cycle
           "C-M-`" #'popper-toggle-type)
  (:option* popper-reference-buffers '("scratch\\*"
                                       "Output\\*$"
                                       "\\*Async Shell Command\\*"
                                       "\\*Xenops-Doctor\\*"
                                       "\\*Emms\\*.*"
                                       "\\*Org LATEX Export\\*"
									   "\\*Pp Macroexpand Output\\*"
                                       emms-browser-mode
                                       compilation-mode)
            popper-mode-line t
            popper-echo-dispatch-keys '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
            popper-display-control nil)
  (:advice popper-raise-popup :after (lambda (&optional _arg)
                                       (delete-other-windows))
		   popper-open-latest :before eli/popper-remove-autoscratch))

;;;; shackle
(setup shackle
  (:once (list :hooks 'find-file-hook)
    (shackle-mode))
  (:option* shackle-rules '(("*Messages*" :align below :size 0.3 :select t)
			                (".*scratch\\*$" :regexp t :select t :align right)
			                (helpful-mode :select t :align right)
							(help-mode :select t :align right)
			                (elfeed-show-mode :select t :align bottom :size 0.85)
							("\\*Pp Macroexpand Output\\*" :regexp t :select t)
			                ("\\*Outline.*\\*" :regexp t :align right :select t :size 0.3)
			                ("*WordNut*" :select t :align right :size 0.4)
			                ("\\*Emms\\*.*" :regexp t:align right :select t :size 0.5)
			                (emms-browser-mode :select t :align right :size 0.5)
							(emacs-lisp-compilation-mode :ignore t)
			                (org-agenda-mode :select t)
			                ("*Org Select*" :select t :align right :size 0.3)
							("*Reconcile*" :select t :align right :size 0.5))))

;;;; gcmh
(setup gcmh
  (:once (list :hooks 'pre-command-hook)
	(gcmh-mode))
  (:option*
   gcmh-high-cons-threshold 1073741824
   gcmh-verbose nil
   gcmh-idle-delay 5)
  (:when-loaded
	(add-function :after after-focus-change-function #'garbage-collect)))

;;;; ace-window
(setup ace-window
  (:global
   "M-o" ace-window)
  (:option*
   aw-scope 'frame))
;;;; cal-china-x
(setup cal-china-x
  (:autoload cal-china-x-setup)
  (:after calendar
    (cal-china-x-setup))
  (:option* calendar-mark-holidays-flag t
            cal-china-x-important-holidays cal-china-x-chinese-holidays
            cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")
                                           (holiday-lunar 7 7 "七夕节")
                                           (holiday-fixed 3 8 "妇女节")
                                           (holiday-fixed 3 12 "植树节")
                                           (holiday-fixed 5 4 "青年节")
                                           (holiday-fixed 6 1 "儿童节")
                                           (holiday-fixed 9 10 "教师节"))
            holiday-other-holidays '((holiday-fixed 2 14 "情人节")
                                     (holiday-fixed 4 1 "愚人节")
                                     (holiday-fixed 12 25 "圣诞节")
                                     (holiday-float 5 0 2 "母亲节")
                                     (holiday-float 6 0 3 "父亲节")
                                     (holiday-float 11 4 4 "感恩节"))
            calendar-holidays (append cal-china-x-important-holidays
                                      cal-china-x-general-holidays
                                      holiday-other-holidays)))

;;;; rime
(setup rime
  (:iload rime)
  (:when-loaded
	(require 'lib-rime))
  (:once (list :hooks 'pre-command-hook)
	(setq default-input-method "rime"))
  (:option*
   rime-user-data-dir "~/.emacs.d/var/rime/"
   rime-disable-predicates '(rime-predicate-prog-in-code-p
				             rime-predicate-space-after-ascii-p
				             rime-predicate-after-ascii-char-p
				             +rime-predicate-punctuation-line-begin-p
				             rime-predicate-org-in-src-block-p
				             rime-predicate-space-after-cc-p
				             rime-predicate-current-uppercase-letter-p
				             rime-predicate-hydra-p
				             rime-predicate-after-latin-char-p)
   rime-show-candidate 'nil
   rime-inline-ascii-trigger 'shift-l
   rime-deactivate-when-exit-minibuffer nil)
  (:when-loaded
	(:global
	 "C-s-k" rime-inline-ascii
	 "C-s-j" +rime-convert-string-at-point)))

;;;; avy
(setup avy
  (:when-loaded
	(ace-pinyin-global-mode))
  (:init
   (defun avy-goto-char-near-point (char)
	 "Jump to the currently visible CHAR in the few lines near point."
	 (interactive (list (read-char "char: " t)))
	 (let ((avy-all-windows nil))
       (avy-with avy-goto-char
		 (avy-process
		  (avy--regex-candidates
		   (regexp-quote (string char))
		   (line-beginning-position -1)
		   (line-end-position 3))
		  (avy--style-fn avy-style))))))
  (:bind-into org-mode-map
	"<remap> <org-cycle-agenda-files>" avy-goto-char)
  (:global
   "C-:" avy-goto-char-in-line
   "C-'" avy-goto-char
   "C-\"" avy-goto-char-near-point))

;;;; GPT
;; (setup doctor-chatgpt
;;   (:iload doctor-chatgpt)
;;   (:bind "C-g" eli/doctor-chatgpt-quit)
;;   (:global
;;    "s-p" doctor-chatgpt-pop-posframe-toggle))

(setup gptel
  (:iload gptel-curl)
  (:also-load
   lib-gptel)
  (:when-loaded
	(toggle-word-wrap))
  (:option*
   gptel-stream t
   ;; gptel-host "api.openai.com"
   gptel-host "api.openai-sb.com"
   gptel-default-mode 'org-mode
   gptel-temperature 0.7
   gptel-prompt-prefix-alist `((markdown-mode . "### ")
							   (org-mode . ,(concat (make-string 125 ?\-) "\n"))
							   (text-mode . "### "))
   gptel-directives '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
					  (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
					  (emacs . "You are an expert in Emacs.")))
  (:hook visual-fill-column-mode
		 visual-line-mode)
  (:advice gptel--create-prompt :override eli/gptel--create-prompt
		   gptel-send :override eli/gptel-send)
  (:bind
   "s-p" eli/gptel-close)
  (:with-feature elfeed
	(:bind-into elfeed-show-mode-map
	  "s" eli/gptel-summary))
  (:with-feature pdf-tools
	(:bind-into pdf-view-mode-map
	  "<mouse-8>" eli/gptel-translate))
  (:global
   "s-p" eli/gptel-posframe-toggle
   "C-c DEL" gptel-abort
   "s-;" gptel-send
   "s-\[" eli/gptel-toggle-last-posframe))

;;;; desktop
(setup desktop
  (:option*
   desktop-dirname "~/.emacs.d/var/desktop/"))

;;;; tab-bar
(setup tab-bar
  (:once (list :hooks 'post-command-hook)
	(:option*
	 tab-bar-close-button-show nil
	 tab-bar-separator " "
	 tab-bar-tab-hints t
	 tab-bar-new-tab-choice "*scratch*"
	 tab-bar-select-tab-modifiers '(super)
	 tab-bar-tab-name-truncated-max 15
	 tab-bar-border nil
	 tab-bar-auto-width nil
	 tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right)
	 tab-bar-tab-name-function #'tab-bar-tab-name-truncated
	 tab-bar-tab-name-format-function (lambda (tab i)
										(concat
										 (propertize (if tab-bar-tab-hints (format "%d" i) "")
													 'face 'tab-bar-hints)
										 (propertize (if tab-bar-tab-hints " " "")
													 'face (funcall #'eli/tab-bar-tab-space-face tab))
										 (propertize (alist-get 'name tab)
													 'face (funcall tab-bar-tab-face-function tab))))))

  (:global
   "s--" tab-bar-close-tab
   "s-=" tab-bar-new-tab
   "s-<left>" tab-bar-move-tab-backward
   "s-<right>" tab-bar-move-tab))

(setup project
  (:option*
   project-vc-merge-submodules nil
   project-list-file "~/.emacs.d/var/projects"))

(setup request
  (:option*
   request-storage-directory "~/.emacs.d/var/request/"))

(setup transient
  (:option*
   transient-history-file "~/.emacs.d/var/transient/history.el"
   transient-levels-file "~/.emacs.d/var/transient/levels.el"
   transient-values-file "~/.emacs.d/var/transient/values.el"))

(setup tabspaces
  (:once (list :before 'tab-bar-new-tab)
	(tabspaces-mode))
  (:also-load
   project
   lib-tab-bar)
  (:hook my--consult-tabspaces)
  (:option*
   tabspaces-session nil))

(setup markdown-mode
  (:option*
   markdown-fontify-code-blocks-natively t))

(setup whisper
  (:option*
   whisper-install-directory "~/src/Clone/"
   whisper-model "base.en"
   whisper-language "en"
   whisper-translate nil
   whisper-enable-speed-up nil))

;;;; keycast
(setup keycast
  (:option*
   keycast-mode-line-insert-after '(:eval (mood-line-segment-misc-info))
   keycast-mode-line-format "%1s%k%c%R"))
;;;; ledger
(setup ledger-mode
  (:also-load lib-ledger-mode)
  (:option* ledger-reconcile-default-commodity "¥"
			ledger-post-amount-alignment-column 80
			ledger-report-auto-refresh-sticky-cursor t
			ledger-report-auto-refresh t
			ledger-copy-transaction-insert-blank-line-after t
			ledger-reconcile-buffer-line-format "%(date)s %-4(code)s %-30(payee)s %-30(account)s %15(amount)s\n"
			ledger-reports '(("bal" "%(binary) -f %(ledger-file) bal")
							 ("bal this month" "%(binary) -f %(ledger-file) bal -p %(month) -S amount")
							 ("bal this year" "%(binary) -f %(ledger-file) bal -p 'this year'")
							 ("net worth"      "%(binary) -f %(ledger-file) bal Assets Liabilities")
							 ("reg" "%(binary) -f %(ledger-file) reg")
							 ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
							 ("account" "%(binary) -f %(ledger-file) reg %(account)")))
  (:when-loaded
	(setq-default ledger-occur-use-face-shown nil))
  (:bind
   "C-M-p" ledger-navigate-previous-uncleared
   "C-M-n" ledger-navigate-next-uncleared
   "C-M-a" ledger-navigate-beginning-of-xact
   "C-M-e" ledger-navigate-end-of-xact
   "C-M-f" eli/jump-to-amount)
  (:advice
   ledger-read-date :override eli/ledger-read-date)
  (:hooks
   ledger-mode-hook eli/ledger-set-tab-style))

;;;; Goldendict
(defun eli/goldendict-word-at-point ()
  "Search word in goldendict"
  (interactive)
  (call-process-shell-command (concat "goldendict " (current-word) " &") nil 0))
(setup simple
  (:global
   "s-h" eli/goldendict-word-at-point))

(setup wordnut
  (:global
   "C-c y" wordnut-lookup-current-word))
;;;; autoscratch
(setup autoscratch
  (:also-load
   lib-autoscratch)
  (:init
   (run-with-idle-timer 0.6 nil (lambda ()
								  (with-current-buffer (get-buffer "*scratch*")
									(autoscratch-mode)))))
  (:option*
   initial-major-mode 'autoscratch-mode
   autoscratch-triggers-alist '(("[(;]" lisp-interaction-mode)
								("#" autoscratch-select
								 '(("python" python-mode)
								   ("shell" shell-script-mode)))
								("[-a-zA-Z0-9]" org-mode)
								("/" c-mode)
								("." fundamental-mode)))
  (:advice
   autoscratch--fork-and-rename-current
   :override eli/autoscratch--fork-and-rename-current)
  (:bind
   [remap yank] eli/autoscratch--yank))
;;;; provide
(provide 'init-misc)
;;; init-misc.el ends here.
