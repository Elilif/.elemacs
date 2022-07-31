;; init-better-defaults.el --- Initialize better-defaults configurations.	-*- lexical-binding: t -*-

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

;; pdf cache setting
(setq image-cache-eviction-delay 60)

(setq word-wrap-by-category t)
(setq help-at-pt-display-when-idle t)

;; set fill column
(setq-default fill-column 80)

;; open customize group buffers in one buffer
(defadvice custom-buffer-create (before my-advice-custom-buffer-create)
  "Exit the current Customize buffer before creating a new one, unless there are modified widgets."
  (if (eq major-mode 'Custom-mode)
      (let ((custom-buffer-done-kill t)
            (custom-buffer-modified nil))
        (mapc (lambda (widget)
                (and (not custom-buffer-modified)
                     (eq (widget-get widget :custom-state) 'modified)
                     (setq custom-buffer-modified t)))
              custom-options)
        (if (not custom-buffer-modified)
            (Custom-buffer-done)))))
(ad-activate 'custom-buffer-create)
(setq custom-buffer-done-kill t)

(add-hook 'elemacs-first-file-hook #'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t)

;; disable backup
(setq make-backup-files nil)
(setq auto-save-default nil)


(setq initial-frame-alist '((fullscreen . maximized)))
(add-hook 'emacs-lisp-mode-hook #'show-paren-mode)


(add-hook 'elemacs-first-buffer-hook #'delete-selection-mode)
(add-hook 'elemacs-first-buffer-hook #'global-hl-line-mode)
(add-hook 'elemacs-first-buffer-hook #'(lambda () (blink-cursor-mode -1)))

(setq auto-save-list-file-prefix nil)

;; http://emacs.stackexchange.com/questions/1051/copy-region-from-emacs-without-newlines
;; improve copy
(defun my-copy-simple (&optional beg end)
  "Save the current region (or line) to the `kill-ring' after stripping extra whitespace and new lines"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((my-text (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (insert my-text)
      (goto-char 1)
      (while (looking-at "[ \t\n]")
        (delete-char 1))
      (let ((fill-column 9333999))
        (fill-region (point-min) (point-max)))
      (kill-region (point-min) (point-max)))))

;; use proxy
(setq url-proxy-services '(
                           ("http" . "127.0.0.1:7890")
                           ("https" . "127.0.0.1:7890")
                           ;; ("socks5" . "127.0.0.1:7891")
			   ))

;; improve hippie-expand
(setq hippie-expand-try-function-list '(try-expand-debbrev
					try-expand-debbrev-all-buffers
					try-expand-debbrev-from-kill
					try-complete-file-name-partially
					try-complete-file-name
					try-expand-all-abbrevs
					try-expand-list
					try-expand-line
					try-complete-lisp-symbol-partially
					try-complete-lisp-symbol))
(global-set-key (kbd "s-/") 'hippie-expand)

(setq use-short-answers t)

(setq load-prefer-newer t)

;; show minibuffer depth
(minibuffer-depth-indicate-mode 1)

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
	    (buffer-substring-no-properties
	     (region-beginning)
	     (region-end))
	  (let ((sym (thing-at-point 'symbol)))
	    (when (stringp sym)
	      (regexp-quote sym))))
	regexp-history)
  (call-interactively 'occur))
(keymap-global-set "M-s o" #'occur-dwim)
(define-key occur-mode-map (kbd "q") 'kill-this-buffer)

;; use winner-mode
(defun transient-winner-undo ()
  "Transient version of winner-undo."
  (interactive)
  (let ((echo-keystrokes nil))
    (winner-undo)
    (message "Winner: [u]ndo [r]edo")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map [?u] #'winner-undo)
       (define-key map [?r] #'winner-redo)
       map)
     t)))
(add-hook 'after-init-hook #'winner-mode)
(keymap-global-set "C-c u" #'transient-winner-undo)

(elemacs-require-package 'avy)
(defun avy-goto-char-near-point (char)
  "Jump to the currently visible CHAR in the few lines near point."
  (interactive (list (read-char "char: " t)))
  (let ((avy-all-windows nil))
    (avy-with avy-goto-char
	      (avy--process
	       (avy--regex-candidates
		(regexp-quote (string char))
		(line-beginning-position -1)
		(line-end-position 3))
	       (avy--style-fn avy-style)))))
(keymap-global-set "C-:" #'avy-goto-char-in-line)
(keymap-global-set "C-'" #'avy-goto-char)
(keymap-global-set "C-\"" #'avy-goto-char-near-point)

(add-to-list 'load-path "~/.emacs.d/site-lisp/pinyinlib/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/ace-pinyin/")
(add-hook 'elemacs-first-file-hook #'(lambda ()
				       (require 'ace-pinyin)
				       (ace-pinyin-global-mode)))

(elemacs-require-package 'grab-x-link)

(elemacs-require-package 'expand-region)
(keymap-global-set "C-=" #'er/expand-region)

(elemacs-require-package 'which-key)
(add-hook 'elemacs-first-input-hook #'which-key-mode)
(setq which-key-idle-delay 0.3)


(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-save/")
(add-hook 'elemacs-first-input-hook #' (lambda ()
					(require 'auto-save)
					(auto-save-enable)))
(with-eval-after-load 'auto-save
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-delete-trailing-whitespace t)
  (setq auto-save-idle 2))

;;; dired
(with-eval-after-load 'dired-x
  (keymap-set dired-mode-map "q" #'kill-this-buffer)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (setq dired-listing-switches "-alh")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwim-target t)
  (setq dired-guess-shell-alist-user '(("\\.doc\\'" "wps")
                                       ("\\.docx\\'" "wps")))
  (put 'dired-find-alternate-file 'disabled nil)
  ;; 切换buffer后，立即刷新
  (defadvice switch-to-buffer (after revert-buffer-now activate)
    (if (eq major-mode 'dired-mode)
	(revert-buffer)))

  ;; 执行shell-command后，立即刷新
  (defadvice shell-command (after revert-buffer-now activate)
    (if (eq major-mode 'dired-mode)
	(revert-buffer)))

  ;; 在Bookmark中进入dired buffer时自动刷新
  (setq dired-auto-revert-buffer t))

(with-eval-after-load 'dired-x
  ;; don't remove `other-window', the caller expects it to be there
  (defun dired-up-directory (&optional other-window)
    "Run Dired on parent directory of current directory."
    (interactive "P")
    (let* ((dir (dired-current-directory))
     	   (orig (current-buffer))
     	   (up (file-name-directory (directory-file-name dir))))
      (or (dired-goto-file (directory-file-name dir))
     	  ;; Only try dired-goto-subdir if buffer has more than one dir.
     	  (and (cdr dired-subdir-alist)
     	       (dired-goto-subdir up))
     	  (progn
     	    (kill-buffer orig)
     	     (dired up)
     	     (dired-goto-file dir))))))
;; or (setq dired-kill-when-opening-new-dired-buffer t)


(elemacs-require-package 'all-the-icons)
(elemacs-require-package 'all-the-icons-dired)
(add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
(with-eval-after-load 'dired-x
    (setq all-the-icons-dired-monochrome nil))


(elemacs-require-package 'hungry-delete)
(add-hook 'elemacs-first-input-hook #'global-hungry-delete-mode)
(setq hungry-delete-join-reluctantly t)

(add-hook 'elemacs-first-input-hook #'recentf-mode)
(with-eval-after-load 'recentf
    (setq recentf-auto-cleanup 'never)
    (setq  recentf-exclude
	   '("/home/eli/.emacs.d/.cache/treemacs-persist-at-last-error"
	     "/home/eli/.emacs.d/.cache/treemacs-persist"
	     "\\.txt"
	     "/home/eli/.emacs.d/elpa/*"
	     "/home/eli/.elfeed/index"
	     "/home/eli/.mail/*"
	     ))
    (setq recentf-max-menu-items 50)
    (setq recentf-max-saved-items 50))

(add-hook 'elemacs-first-file-hook #'save-place-mode)

(elemacs-require-package 'popwin)
(add-hook 'elemacs-first-buffer-hook #'popwin-mode)
(with-eval-after-load 'popwin
  (setq popwin:popup-window-position 'right)
  (setq popwin:popup-window-width 80))

(elemacs-require-package 'multiple-cursors)
(with-eval-after-load 'multiple-cursors
  (setq mc/always-run-for-all nil)
  (setq mc/insert-numbers-default 1))

(keymap-global-set "C-x C-b"  #'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("emacs" (or
			 (mode . emacs-lisp-mode)
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
	       ("magit" (or
                         (mode . magit-status-mode)
                         (mode . magit-process-mode)
                         (mode . magit-diff-mode)
                         (mode . magit-revision-mode)
                         (mode . magit-log-mode)))
	       ("org" (mode . org-mode))
	       ))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(elemacs-require-package 'hl-todo)
(add-hook 'elemacs-first-file-hook #'global-hl-todo-mode)

(elemacs-require-package 'helpful)
(keymap-global-set "C-h f" #'helpful-callable)
(keymap-global-set "C-h v" #'helpful-variable)
(keymap-global-set "C-h k" #'helpful-key)
(with-eval-after-load 'helpful
  (setq helpful-max-buffers 2)
  ;; from: https://d12frosted.io/posts/2019-06-26-emacs-helpful.html
  (setq helpful-switch-buffer-function #'+helpful-switch-to-buffer)
  (defun +helpful-switch-to-buffer (buffer-or-name)
    "Switch to helpful BUFFER-OR-NAME. The logic is simple, if we are
currently in the helpful buffer, reuse it's window, otherwise
create new one."
    (if (eq major-mode 'helpful-mode)
	(switch-to-buffer buffer-or-name)
      (pop-to-buffer buffer-or-name)))
  (keymap-set helpful-mode-map "q" #'kill-buffer-and-window))

(defun my-search-with-chrome ()
  "search with chrome."
  (interactive)
  (let ((target (read-string "Search for: ")))
    (browse-url (concat "http://www.google.com/search?q="
			(url-hexify-string target)))))
(setq sentence-end-double-space nil)

(setq kill-whole-line t)

(setq bookmark-set-fringe-mark nil)

(elemacs-require-package 'popper)
(add-hook 'elemacs-first-buffer-hook #'popper-mode)
(add-hook 'elemacs-first-buffer-hook #'popper-echo-mode)
(keymap-global-set "C-`" #'popper-toggle-latest)
(keymap-global-set "M-`" #'popper-cycle)
(keymap-global-set "C-M-`" #'popper-toggle-type)
(with-eval-after-load 'popper
  (setq popper-reference-buffers
	'("\\*Messages\\*"
	  "\\*scratch\\*"
	  "Output\\*$"
	  "\\*Async Shell Command\\*"
	  "\\*Xenops-Doctor\\*"
	  "\\*Emms\\*.*"
	  "\\*Org LATEX Export\\*"
	  emms-browser-mode
	  org-agenda-mode
	  helpful-mode
	  compilation-mode))
  (setq popper-mode-line t)
  (setq popper-echo-dispatch-keys '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (setq popper-display-control nil))
(add-hook 'elemacs-first-buffer-hook #'popper-mode)
(add-hook 'elemacs-first-buffer-hook #'popper-echo-mode)

(elemacs-require-package 'shackle)
(with-eval-after-load 'popper
  (setq shackle-rules '(("*Messages*" :align below :size 0.3 :select t)
			("*scratch*" :select t :align right)
			(helpful-mode :select t :align right)
			(elfeed-show-mode :select t :align right :size 0.75)
			("\\*Outline.*\\*" :regexp t :align right :select t :size 0.3)
			("*WordNut*" :select t :align right :size 0.4)
			("\\*Emms\\*.*" :regexp t:align right :select t :size 0.5)
			(emms-browser-mode :select t :align right :size 0.5)
			(org-agenda-mode :select t :align right :size 0.35)
			("*Org Select*" :select t :align right :size 0.3)
			)))
(add-hook 'elemacs-first-buffer-hook #'shackle-mode)


;; Chinese calendar
;; `pC' can show lunar details
(elemacs-require-package 'cal-china-x)
(with-eval-after-load 'calendar
  (require 'cal-china-x)
  (cal-china-x-setup)
  ;; Holidays
  (setq calendar-mark-holidays-flag t
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


(elemacs-require-package 'ledger-mode)
(with-eval-after-load 'ledger
  (setq ledger-reconcile-default-commodity "¥"
	ledger-post-amount-alignment-column 80
	ledger-report-auto-refresh-sticky-cursor t
	ledger-report-auto-refresh t
	ledger-copy-transaction-insert-blank-line-after t)
  (setq-default ledger-occur-use-face-shown nil))

(provide 'init-better-defaults)
;;; init-better-defaults.el ends here.
