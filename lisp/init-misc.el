;; init-misc.el --- Initialize misc configurations.     -*- lexical-binding: t -*-

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
  (require 'lib-tab-bar)
  (require 'lib-burly)
  (require 'elfeed-show)
  (require 'pdf-tools))

;;;; dired
(setup dired
  (:once (list :packages 'dired)
    (diredfl-global-mode))
  (:hook nerd-icons-dired-mode))

(setup dirvish
  (:once (list :packages 'dired)
    (dirvish-override-dired-mode))
  (:option*
   dirvish-attributes '(file-size)
   dirvish-mode-line-height 20
   dirvish-use-mode-line t
   dirvish-quick-access-entries '(("h" "~/" "Home")
                                  ("e" "~/.emacs.d/lisp/" "Emacs user directory")
                                  ("l" "~/.emacs.d/site-lisp/" "Emacs site-lisp directory")))
  (:with-feature dired
    (:bind
     "s" dirvish-quicksort
     "a" dirvish-quick-access
     "f" dirvish-file-info-menu
     "y" dirvish-yank-menu
     "l" dirvish-history-go-backward
     "r" dirvish-history-go-forward
     "h" dirvish-history-jump
     "TAB" dirvish-subtree-toggle
     "G" dirvish-emerge-menu)))

;;;; ibuffer
(setup ibuffer
  (:hook nerd-icons-ibuffer-mode))


;;;; hl-todo
(setup hl-todo
  (:once (list :hooks 'find-file-hook)
    (global-hl-todo-mode)))

;;;; popper
(setup popper
  (:once (list :hooks 'find-file-hook
               :before 'eli/pop-to-buffer)
    (popper-mode)
    (popper-echo-mode))
  (:autoload popper-toggle-latest)
  (:autoload popper-cycle)
  (:autoload popper-toggle-type)
  (:global "C-`" #'popper-toggle-latest
           "M-`" #'popper-cycle
           "C-M-`" #'popper-toggle-type)
  (:option* popper-reference-buffers '(emms-playlist-mode
                                       emms-browser-mode)
            popper-mode-line t
            popper-echo-dispatch-keys '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
            popper-display-control nil)
  (:advice popper-raise-popup :after (lambda (&optional _arg)
                                       (delete-other-windows))
           popper-open-latest :before eli/popper-remove-autoscratch))

;;;; shackle
(setup shackle
  (:once (list :hooks 'find-file-hook
               :before 'eli/pop-to-buffer)
    (shackle-mode))
  (:option* shackle-rules '(("*Messages*" :align below :size 0.3 :select t)
                            (".*scratch\\*$" :regexp t :select t :align right)
                            (helpful-mode :select t :align right)
                            (help-mode :select t :align right)
                            (souyun-mode :select t :align right)
                            (elfeed-show-mode :select t :align bottom :size 0.85)
                            ("*Pp Macroexpand Output*" :select t)
                            ("\\*Outline.*\\*" :regexp t :align right :select t :size 0.3)
                            ("*WordNut*" :select t :align right :size 0.4)
                            (emms-playlist-mode :align right :select t :size 0.4)
                            (emms-browser-mode :select t :align right :size 0.4)
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
   "M-o" ace-window
   "s-j" previous-window-any-frame
   "s-k" next-window-any-frame
   "s-b" (lambda () (interactive) (windmove-left 1))
   "s-f" (lambda () (interactive) (windmove-right 1))
   "s-n" (lambda () (interactive) (windmove-down 1))
   "s-p" (lambda () (interactive) (windmove-up 1)))
  (:option*
   aw-scope 'frame
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
;;;; cal-china-x
(setup cal-china-x
  (:autoload cal-china-x-setup)
  (:after calendar
    (cal-china-x-setup))
  (:option* calendar-mark-holidays-flag nil
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
                             rime-predicate-org-latex-mode-p
                             +rime-predicate-punctuation-line-begin-p
                             rime-predicate-org-in-src-block-p
                             rime-predicate-space-after-cc-p
                             rime-predicate-current-uppercase-letter-p
                             rime-predicate-hydra-p
                             eli/rime-predicate-org-in-verbatim-emphasis-p
                             +rime-predicate-cc-notes-p
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
  (:after ace-pinyin
    (defun ace-pinyin-jump-char-in-line (char)
      "Ace-pinyn replacement of `avy-goto-char-in-line'."
      (interactive (list (read-char "char: ")))
      (avy-with avy-goto-char
        (avy-jump
         (ace-pinyin--build-regexp char nil)
         :window-flip avy-all-windows
         :beg (line-beginning-position)
         :end (line-end-position)))))
  (:with-feature org
    (:bind
     "<remap> <org-cycle-agenda-files>" avy-goto-char-timer))
  (:option*
   avy-all-windows t
   avy-timeout-seconds 0.5)
  (:global
   "C-;" avy-goto-char-in-line
   "C-'" avy-goto-char-timer))

;;;; GPT
(setup gptel
  (:iload gptel)
  (:also-load
   lib-gptel
   gptel-transient)
  (:autoload gptel-mode)
  (:init
   (add-to-list 'auto-mode-alist '("\\.chat\\'" . org-mode)))
  (:when-loaded
    (toggle-word-wrap)
    (:hooks
     kill-emacs-hook eli/gptel-save-conversations))
  (:once (list :before 'eli/gptel-posframe-toggle)
    (eli/gptel-restore-conversations))
  (:option*
   gptel-model "gpt-3.5-turbo-16k"
   gptel-crowdsourced-prompts-file "~/.emacs.d/etc/gptel/gptel-crowdsourced-prompts.csv"
   gptel-stream t
   gptel-backend (gptel-make-openai "opai-sb"
                                    :protocol "https"
                                    :host "api.openai-sb.com"
                                    :stream t
                                    :key 'gptel-api-key
                                    :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
                                    :models '("gpt-3.5-turbo-16k" "gpt-4"))
   ;; gptel-proxy "socks://127.0.0.1:7891"
   gptel-default-mode 'org-mode
   gptel-temperature 0.3
   gptel-prompt-prefix-alist `((markdown-mode . "### ")
                               (org-mode . ,(concat (make-string 110 ?\-) "\n"))
                               (text-mode . "### "))
   gptel-response-prefix-alist `((markdown-mode . "")
                                 (org-mode . ,(concat (make-string 55 ?\-) "\n"))
                                 (text-mode . ""))
   gptel-directives '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
                      (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
                      (emacs . "You are an expert in Emacs.")))
  (:hook visual-fill-column-mode
         visual-line-mode
         gptel-posframe-mode)
  (:advice
   gptel--create-prompt :around eli/gptel--create-prompt)
  (:bind
   "s-p" eli/gptel-close
   "s-i" eli/gptel-read-crowdsourced-prompt
   "s-\]" eli/gptel-close
   "C-c C-k" eli/gptel-clean
   "C-c C-n" eli/gptel-narrow)
  (:with-feature elfeed-show
    (:bind
     "s" eli/gptel-summary))
  (:with-feature pdf-view
    (:bind
     "<mouse-8>" eli/gptel-translate))
  (:global
   "s-i" eli/gptel-posframe-toggle
   "C-c DEL" gptel-abort
   "s-;" gptel-send
   "s-\]" eli/gptel-toggle-last-posframe))

;;;; desktop
(setup desktop
  (:option*
   desktop-dirname "~/.emacs.d/var/desktop/"
   desktop-path '("~/.emacs.d/var/desktop/")
   desktop-save t))

;;;; tab-bar
(setup tabspaces
  (:also-load
   lib-tab-bar)
  (:option*
   tab-bar-close-button-show nil
   tab-bar-separator "​​"
   tab-bar-tab-hints t
   tab-bar-new-tab-choice "*scratch*"
   tab-bar-select-tab-modifiers '(super)
   tab-bar-tab-name-truncated-max 15
   tab-bar-border nil
   tab-bar-auto-width nil
   tab-bar-format '(;; eli/tab-bar-icon
                    tab-bar-format-tabs
                    tab-bar-separator
                    tab-bar-format-align-right
                    eli/tab-bar-emms)
   tab-bar-tab-name-function #'tab-bar-tab-name-truncated
   tab-bar-tab-name-format-function #'eli/tab-bar-tab-name-with-svg
   tab-bar-auto-width-max '(200  15))
  (:global
   "s--" eli/tabspaces-kill-buffers-close-workspace
   "s-=" tab-bar-new-tab
   "s-<left>" tab-bar-move-tab-backward
   "s-<right>" tab-bar-move-tab)
  (:hook
   tab-bar-history-mode))

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
    (require 'svg-tag-mode)
    (require 'lib-svg-tag-mode)
    (tabspaces-mode))
  (:also-load
   project
   lib-tab-bar)
  (:hook my--consult-tabspaces)
  (:bind-into tabspaces-mode-map
    "C-c TAB S" tabspaces-save-session)
  (:option*
   tabspaces-session t)
  (:advice
   tabspaces-restore-session :after eli/tabspaces-delete-empty-tab
   tabspaces-save-session :override eli/tabspaces-save-session
   tabspaces-restore-session :override eli/tabspaces-restore-session))

(setup burly
  (:when-loaded
    (require 'lib-burly))
  (:once (list :before 'burly-open-bookmark 'consult-bookmark
               'tabspaces-restore-session 'eli/tabspaces-restore-session)
    (burly-tabs-mode))
  (:option*
   burly-bookmark-prefix nil)
  (:hooks
   org-agenda-finalize-hook (lambda ()
                              (setq-local bookmark-make-record-function
                                          #'eli/org-agenda-bookmark-make-record))
   org-doc-noter-doc-mode-hook (lambda ()
                                 (setq-local bookmark-make-record-function
                                             #'eli/org-doc-noter-bookmark-make-record)))
  (:advice
   burly-open-bookmark :after eli/tabspaces-delete-empty-tab
   eli/burly-bookmark-windows :after eli/burly-rename-tab
   burly-bookmark-names :override eli/burly-bookmark-names))

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

(defvar silverdict--frame nil)

(defun eli/silverdict-pop-posframe-toggle (word)
  "Toggle silverdict in child frame."
  (interactive (if current-prefix-arg
                   (list (read-string "Input a word: "))
                 (cond
                  ((region-active-p)
                   (let ((word (buffer-substring-no-properties
                                (region-beginning)
                                (region-end))))
                     (deactivate-mark)
                     (list word)))
                  (t (list (thing-at-point 'word))))))
  (unless (and silverdict--frame
               (frame-live-p silverdict--frame))
    (let ((width  (max 100 (round (* (frame-width) 0.62))))
          (height (round (* (frame-height) 0.62))))
      (eli/silverdict--query word)
      (setq silverdict--frame
            (posframe-show
             "*silverdict*"
             :poshandler #'posframe-poshandler-frame-center
             :hidehandler nil
             :left-fringe 8
             :right-fringe 8
             :width width
             :height height
             :min-width width
             :min-height height
             :border-width 2
             :border-color "light gray"
             :background-color (face-background 'tooltip nil t)
             :override-parameters '((cursor-type . t))
             :accept-focus t))

      (with-current-buffer "*silverdict*"
        (setq-local cursor-type 'box
                    line-spacing 10)
        (read-only-mode)
        (keymap-local-set "s-p" #'posframe-hide-all))))
  ;; Focus in child frame
  (eli/silverdict--query word)
  (select-frame-set-input-focus silverdict--frame))

(defun eli/silverdict--query (word)
  "Perform silverdict query for WORD."
  (let ((buffer (url-retrieve-synchronously
                 (url-encode-url
                  (concat
                   "http://127.0.0.1:2628/api/query/"
                   "Default Group"
                   "/"
                   word))
                 t))
        (inhibit-read-only t)
        dom)
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward-regexp "^$")
      (setq dom (libxml-parse-html-region (point))))
    (with-current-buffer (get-buffer-create "*silverdict*")
      (erase-buffer)
      (save-excursion
        (shr-insert-document dom)))))
(setup simple
  (:global
   "s-h" eli/silverdict-pop-posframe-toggle))

(setup wordnut
  (:global
   "C-c y" wordnut-lookup-current-word))
;;;; autoscratch
(setup autoscratch
  (:also-load
   lib-autoscratch)
  (:delay 0.6
    (with-current-buffer (get-buffer "*scratch*")
      (autoscratch-mode)
      (eli/scratch-restore)))
  (:option*
   autoscratch-fork-after-trigger nil
   autoscratch-post-trigger-hook '((lambda ()
                                     (when-let ((map (current-local-map)))
                                       (use-local-map (copy-keymap map)))
                                     (keymap-local-set "C-c k" #'eli/scratch-new)
                                     (add-hook 'kill-buffer-query-functions
                                               (lambda ()
                                                 (y-or-n-p
                                                  "Do you want to kill *scratch* buffer?"))
                                               nil t)))
   initial-major-mode 'autoscratch-mode
   autoscratch-trigger-after most-positive-fixnum
   autoscratch-triggers-alist '(("[(;]" lisp-interaction-mode)
                                ("#" autoscratch-select
                                 '(("python" python-mode)
                                   ("shell" shell-script-mode)))
                                ("\\cc\\|[\"*-a-zA-Z0-9]" org-mode)
                                ("/" c-mode)
                                ("." fundamental-mode)))
  (:advice
   autoscratch--fork-and-rename-current
   :override eli/autoscratch--fork-and-rename-current)
  (:bind
   [remap yank] eli/autoscratch--yank)
  (:when-loaded
    (:hooks
     kill-emacs-hook eli/scratch-save)))

(setup immersive-translate
  (:hooks
   elfeed-show-mode-hook immersive-translate-setup
   nov-pre-html-render-hook immersive-translate-setup)
  (:option*
   immersive-translate-backend 'chatgpt
   immersive-translate-baidu-appid "20230720001751104"
   immersive-translate-chatgpt-host "api.openai-sb.com"))

(setup mini-echo
  (:once (list :hooks 'pre-command-hook)
    (require 'lib-mini-echo)
    (mini-echo-mode 1))
  (:advice
   window-divider-mode :override ignore
   mini-echo-hide-modeline :override ignore)
  (:option*
   mini-echo-short-style-predicate #'eli/mini-echo-use-short-style-p
   mini-echo-update-interval nil
   mini-echo-right-padding 3
   mini-echo-default-segments '(:long ("org-clock" "org-todo")
                                      :short nil)))

;;;; beacon
;; (setup beacon
;;   (:once (list :before
;;                 'scroll-up-command 'scroll-down-command)
;;      (beacon-mode))
;;   (:option*
;;    beacon-blink-delay 0.1
;;    beacon-blink-duration 0.2
;;    beacon-blink-when-window-changes nil
;;    beacon-blink-when-buffer-changes nil))
;;;; provide
(provide 'init-misc)
;;; init-misc.el ends here.
