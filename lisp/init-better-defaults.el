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

;; enable recursive minibuffer
(setq enable-recursive-minibuffers t)

;; accept function redefinition warning
(setq ad-redefinition-action 'accept)

;; disable large file warning
(setq large-file-warning-threshold nil)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Tab.space equivalence
(setq-default tab-width 4)

;; pdf cache setting
(setq image-cache-eviction-delay 60)

(setq word-wrap-by-category t)
(setq help-at-pt-display-when-idle t)

;; set fill column
(setq-default fill-column 80)

;; use system trash
(setq delete-by-moving-to-trash t)

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
(add-hook 'elemacs-first-buffer-hook (lambda () (blink-cursor-mode -1)))

(setq auto-save-list-file-prefix nil)

;; use proxy
(setq url-proxy-services '(
                           ("http" . "127.0.0.1:7890")
                           ("https" . "127.0.0.1:7890")
                           ;; ("socks5" . "127.0.0.1:7891")
			   ))

;; improve hippie-expand
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					                     try-expand-dabbrev-all-buffers
					                     try-expand-dabbrev-from-kill
					                     try-complete-file-name-partially
					                     try-complete-file-name
					                     try-expand-all-abbrevs
					                     try-expand-list
					                     try-expand-line
					                     try-complete-lisp-symbol-partially
					                     try-complete-lisp-symbol))
(keymap-global-set "s-/" 'hippie-expand)

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
(add-hook 'occur-hook
          (lambda ()
             (switch-to-buffer-other-window "*Occur*")))

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
(autoload 'ace-pinyin-global-mode "ace-pinyin")
(add-hook 'elemacs-first-input-hook #'ace-pinyin-global-mode)


(keymap-global-set "C-=" #'er/expand-region)
(autoload #'er/mark-outside-pairs "expand-region")
(keymap-global-set "C-+" #'er/mark-outside-pairs)
(with-eval-after-load 'expand-region
  (defun er/mark-block-comment ()
    "Mark the entire comment around point."
    (interactive)
    (when (er--point-is-in-comment-p)
      (let ((p (point)))
        (while (and (er--point-is-in-comment-p) (not (eobp)))
          (forward-char 1)
          (skip-chars-forward "\n\r "))
        (beginning-of-line)
        (backward-char 1)
        (set-mark (point))
        (goto-char p)
        (while (er--point-is-in-comment-p)
          (backward-char 1)
          (skip-chars-backward "\n\r "))
        (forward-char 1))))
  
  (setq-default er/try-expand-list '(er/mark-word
                                     er/mark-symbol
                                     er/mark-symbol-with-prefix
                                     er/mark-next-accessor
                                     er/mark-method-call
                                     er/mark-inside-quotes
                                     er/mark-outside-quotes
                                     er/mark-inside-pairs
                                     er/mark-outside-pairs
                                     er/mark-comment
                                     er/mark-block-comment
                                     er/mark-url
                                     er/mark-email
                                     er/mark-defun
                                     er/mark-sentence
                                     mark-paragraph
                                     ))

  (defun eli/er-clearn-history (_arg)
    (if (not (memq last-command '(er/expand-region er/contract-region)))
        (er/clear-history)))
  (advice-add 'er/expand-region :before #'eli/er-clearn-history))

(with-eval-after-load 'org
  (advice-remove #'exchange-point-and-mark #'org-mark-jump-unhide))

(add-hook 'elemacs-first-input-hook #'which-key-mode)
(setq which-key-idle-delay 0.3)


(autoload #'auto-save-enable "auto-save")
(add-hook 'elemacs-first-input-hook #'auto-save-enable)
(with-eval-after-load 'auto-save
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-delete-trailing-whitespace nil)
  (setq auto-save-idle 2))

;;; dired
(with-eval-after-load 'dired-x
  (keymap-set dired-mode-map "q" #'kill-this-buffer)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (setq dired-listing-switches "-alh"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        ;; 在Bookmark中进入dired buffer时自动刷新
        dired-auto-revert-buffer t
        dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer t
        dired-guess-shell-alist-user '(("\\.doc\\'" "wps")
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
  
  (defun dired-open-externally (&optional arg)
    "Open marked or current file in operating system's default application."
    (interactive "P")
    (dired-map-over-marks
     (consult-file-externally (dired-get-filename))
     arg))
  (keymap-set dired-mode-map "E" #'dired-open-externally)
  )

(add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
(with-eval-after-load 'dired-x
    (setq all-the-icons-dired-monochrome nil))

(add-to-list 'load-path "~/.emacs.d/site-lisp/diredfl/")
(with-eval-after-load 'dired-x
  (require 'diredfl)
  (diredfl-global-mode))

(add-hook 'elemacs-first-input-hook #'global-hungry-delete-mode)
(setq hungry-delete-join-reluctantly t)

(add-hook 'elemacs-first-input-hook #'recentf-mode)
(with-eval-after-load 'recentf
    (setq recentf-auto-cleanup 'never)
    (setq recentf-exclude
	   '("/home/eli/.emacs.d/.cache/treemacs-persist-at-last-error"
	     "/home/eli/.emacs.d/.cache/treemacs-persist"
	     "\\.txt"
	     "/home/eli/.emacs.d/elpa/*"
	     "/home/eli/.elfeed/index"
	     "/home/eli/.mail/*"
         "/tmp/*"
	     ))
    (setq recentf-max-menu-items 50)
    (setq recentf-max-saved-items 50))

(add-hook 'elemacs-first-file-hook #'save-place-mode)

(add-hook 'elemacs-first-buffer-hook #'popwin-mode)
(with-eval-after-load 'popwin
  (setq popwin:popup-window-position 'right)
  (setq popwin:popup-window-width 80))

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

(with-eval-after-load 'all-the-icons-ibuffer-mode
  (setq all-the-icons-ibuffer-icon t)
  (setq all-the-icons-ibuffer-color-icon t))
(add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode)

(add-hook 'elemacs-first-file-hook #'global-hl-todo-mode)

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

  (keymap-set helpful-mode-map "q" #'kill-buffer-and-window)
  (defun helpful-set-arguments-face (&rest _args)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil)
            (limit (save-excursion
                     (re-search-forward "^Source Code\n" nil t))))
        (while (re-search-forward
                "\\<[-A-Z]+\\>" limit t)
          (overlay-put (make-overlay
                        (match-beginning 0) (match-end 0))
                       'face 'help-argument-name)))))

  (advice-add 'helpful--update-and-switch-buffer :after  #'helpful-set-arguments-face))

(defun my-search-with-chrome ()
  "search with chrome."
  (interactive)
  (let ((target (read-string "Search for: ")))
    (browse-url (concat "http://www.google.com/search?q="
			            (url-hexify-string target)))))
(setq sentence-end-double-space nil)

(setq kill-whole-line t)

(setq bookmark-set-fringe-mark nil)

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
  (setq popper-echo-dispatch-keys '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (setq popper-display-control nil)
  (advice-add 'popper-raise-popup :after (lambda (&optional _arg)
                                           (delete-other-windows))))

(with-eval-after-load 'popper
  (setq shackle-rules '(("*Messages*" :align below :size 0.3 :select t)
			            ("*scratch*" :select t :align right)
			            (helpful-mode :select t :align right)
			            (elfeed-show-mode :select t :align bottom :size 0.85)
			            ("\\*Outline.*\\*" :regexp t :align right :select t :size 0.3)
			            ("*WordNut*" :select t :align right :size 0.4)
			            ("\\*Emms\\*.*" :regexp t:align right :select t :size 0.5)
			            (emms-browser-mode :select t :align right :size 0.5)
			            (org-agenda-mode :select t)
			            ("*Org Select*" :select t :align right :size 0.3)
			            )))
(add-hook 'elemacs-first-buffer-hook #'shackle-mode)


;; Chinese calendar
;; `pC' can show lunar details
(autoload #'cal-china-x-setup "cal-china-x")
(with-eval-after-load 'calendar
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


;;; enable call emacs functions from outside
;;; from https://isamert.net/2022/03/16/global-interactive-emacs-functions.html

(defvar elemacs-global-interactive-defer-to-system-app nil)

(defvar elemacs-global-interactive-commands nil)


(defun elemacs-global-interactive-system-read-string (prompt)
  "Like `read-string' but use an Emacs independent system level app
to get user input. You need to install `zenity'."
  (string-trim
   (shell-command-to-string
    (format "zenity --entry --text='%s'" prompt))))

(advice-add #'read-string :around (lambda (orig-fun prompt &rest args)
                                    (if elemacs-global-interactive-defer-to-system-app
                                        (elemacs-global-interactive-system-read-string prompt)
                                      (apply orig-fun prompt args))))

(defun elemacs-global-interactive-dmenu (prompt items &rest ignored)
  "Like `completing-read' but instead use dmenu.
Useful for system-wide scripts."
  (with-temp-buffer
    (thread-first
      (cond
       ((functionp items)
        (funcall items "" nil t))
       ((listp (car items))
        (mapcar #'car items))
       (t
        items))
      (string-join "\n")
      string-trim
      insert)
    (shell-command-on-region
     (point-min)
     (point-max)
     (pcase system-type
       ('gnu/linux (format "rofi -dmenu -fuzzy -i -p '%s' -theme $HOME/.config/rofi/carbonized/config.rasi" prompt))
       ('darwin "choose"))
     nil t "*elemacs-global-interactive-dmenu error*" nil)
    (string-trim (buffer-string))))

(defun elemacs-global-interactive-run ()
  "enable selecting a functions from `elemacs-global-interactive-commands' and call it."
  (let*
      ((completing-read-function #'elemacs-global-interactive-dmenu)
       (elemacs-global-interactive-defer-to-system-app t)
       (candidates (mapcar #'symbol-name  elemacs-global-interactive-commands))
       (selected-item (completing-read "Select: " candidates)))
    (unless (string-empty-p selected-item)
      (funcall (intern selected-item)))))


;;; support call org-capture from outside.
(defvar elemacs-global-interactive-capture-p nil
  "Non-nil if call `org-capture' through `elemacs-global-interactive--capture'.")

(defun elemacs-global-interactive-capture ()
  "For use as an `org-capture-templates-contexts'."
  elemacs-global-interactive-capture-p)

(defun elemacs-global-interactive--capture ()
  "Call `org-capture' from outside."
  (let* ((key-alist '(("Note" . "1")
                      ("TODO" . "2")))
         (key (cdr (assoc (completing-read "Input: " '("Note" "TODO")) key-alist)))
         (elemacs-global-interactive-capture-p t)
         (org-capture-initial (when key
                                (completing-read "Input: " nil))))
    (when (and key (not (string-empty-p org-capture-initial)))
      (org-capture nil key))))

;;; override the original function to enable global interactive calling.
(with-eval-after-load 'consult-emms
  (defun consult-emms--playlist (buffer)
    "Select a track from EMMS buffer BUFFER.

BUFFER is a string, the name of a buffer."
    ;; `consult-emms--playlist-source-from-buffer' does most of the work
    ;; of forming the args for us, and it's a good idea to avoid code
    ;; duplication, so we use it here. BUT, it forms a source for
    ;; `consult--multi', which is different from the arg list taken by
    ;; `consult--read', so we have to transform it a bit.
    (let* ((raw-args (consult-emms--playlist-source-from-buffer buffer))
	       (items (plist-get raw-args :items))
	       (action (plist-get raw-args :action))
	       ;; TODO Get this list programatically
	       (allowed '(:prompt :predicate :require-match ;; Keywords in `consult--read'
		                      :history :default :keymap
		                      :category :initial :narrow
		                      :add-history :annotate :state
		                      :preview-key :sort :group
		                      :inherit-input-method))
	       ;; Use only arg keys used by `consult--read'
	       (filtered-args (cl-loop for (key value) on raw-args by 'cddr
				                   if (member key allowed)
				                   collect key and collect value))
	       (read-args (append `(:prompt ,(format "EMMS playlist <%s>: " buffer))
			                  filtered-args))
	       ;; Lots of the actions use text properties as variables, so
	       ;; make sure they persist through minibuffer choice
	       (minibuffer-allow-text-properties t)
	       (raw-track (apply 'consult--read `(,items ,@read-args)))
           (track (cl-loop for item in items
                           until (string= item raw-track)
                           finally return item)))
      ;; Using the action extracted above guarantees that the behaviour
      ;; will be the same as with the corresponding source
      (unless (string-empty-p raw-track)
        (funcall action track)))))

(setq elemacs-global-interactive-commands '(org-mru-clock-in
                                            elemacs-global-interactive--capture
                                            consult-emms-current-playlist))

;; another choice is `info-pretty-mode'
(add-hook 'Info-mode-hook #'variable-pitch-mode)
(with-eval-after-load 'info
  ;; copy info url
  (defvar eli/Info-url-alist
    '(("org" . "https://orgmode.org/manual/")
      ("emacs" . "https://www.gnu.org/software/emacs/manual/html_node/emacs/")
      ("elisp" . "https://www.gnu.org/software/emacs/manual/html_node/elisp/"))
    "Official manual URL for `eli/Info-url-for-node'.")

  (defun eli/Info-url-for-node (node)
    "Return a URL for NODE.

NODE should be a string on the form \"(manual)Node\"."
    (unless (string-match "\\`(\\(.+\\))\\(.+\\)\\'" node)
      (error "Invalid node name %s" node))
    (let ((manual (match-string 1 node))
          (node (match-string 2 node)))
      ;; Encode a bunch of characters the way that makeinfo does.
      (setq node
            (mapconcat (lambda (ch)
                         (if (or (< ch 32)        ; ^@^A-^Z^[^\^]^^^-
                                 (<= 33 ch 47)    ; !"#$%&'()*+,-./
                                 (<= 58 ch 64)    ; :;<=>?@
                                 (<= 91 ch 96)    ; [\]_`
                                 (<= 123 ch 127)) ; {|}~ DEL
                             (format "_00%x" ch)
                           (char-to-string ch)))
                       node
                       ""))
      (concat (cdr (assoc manual eli/Info-url-alist #'equal))
              (url-hexify-string (string-replace " " "-" node))
              ".html")))

  (defun eli/Info-copy-node-url (node)
    "Put the online url of the current Info NODE into the kill ring.

By default, go to the current Info node."
    (interactive (list (Info-read-node-name
                        "Go to node (default current page): " Info-current-node))
                 Info-mode)
    (kill-new
     (eli/Info-url-for-node (format "(%s)%s" (file-name-sans-extension
                                              (file-name-nondirectory
                                               Info-current-file))
                                    node))))

  (keymap-set Info-mode-map "C" #'eli/Info-copy-node-url))

;;; better movement
(keymap-global-set "C-a" #'mwim-beginning)
(keymap-global-set "C-e" #'mwim-end)

;; automatically select apropos buffer
(add-hook 'apropos-mode-hook (lambda ()
                               (switch-to-buffer-other-window "*Apropos*")))

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(keymap-global-set "M-DEL" #'backward-kill-sentence)

;; insert key-sequence
(defun eli/insert-key-sequence ()
  (interactive)
  (insert (key-description
           (read-key-sequence-vector "Press a keystrokes:"))))

(defun smart-align (beg end)
  (interactive "r")
  (with-demoted-errors
      "Something wrong when align."
    (let ((align-start
           (or beg (save-excursion
                     (backward-up-list)
                     (point))))
          (align-end
           (or end (save-excursion
                     (up-list)
                     (point)))))
      (align-regexp align-start align-end "\\(\\s-*\\)\\(=\\|:\\)" 1 1))))

;;; smart-mark
(add-hook 'elemacs-first-input-hook #'smart-mark-mode)

;;; keyfreq
(with-eval-after-load 'hydra
  (setq keyfreq-file "~/.emacs.d/.emacs.keyfreq")
  (setq keyfreq-file-lock "~/.emacs.d/.emacs.keyfreq.lock")
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(self-insert-command
          forward-char
          backward-char
          previous-line
          next-line)))

;;; ace window
(keymap-global-set "M-o" #'ace-window)
(with-eval-after-load 'ace
  (setq aw-reverse-frame-list t
        aw-dispatch-always nil
        aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;; edit
(with-eval-after-load 'hydra
  (setq markmacro-secondary-region-mark-cursors-type 'symbol)
  (keymap-global-set "s-=" 'markmacro-apply-all)
  (keymap-global-set "s--" 'kmacro-start-macro)
  (keymap-global-set "s-w" 'markmacro-mark-words)
  (keymap-global-set "s-l" 'markmacro-mark-lines)
  (keymap-global-set "C-r" 'rectangle-mark-mode)
  (keymap-global-set "s-g" 'markmacro-secondary-region-set)
  (keymap-global-set "s-a" 'markmacro-secondary-region-mark-cursors)
  (keymap-global-set "s-s" 'markmacro-swap-region)
  (keymap-global-set "s-f" #'markmacro-mark-current-or-next-target)
  (keymap-global-set "s-b" #'markmacro-mark-current-or-previous-target)
  (keymap-global-set "s-u" #'markmacro-unmark-current-target)


  (defun eli/speed-up-kmacro (_args)
    (smartparens-mode -1)
    (remove-hook 'post-self-insert-hook
                 #'my/yas-try-expanding-auto-snippets)
    (winner-mode -1)
    (hungry-delete-mode -1)
    (flyspell-mode -1)
    (aggressive-indent-mode -1)
    (corfu-mode -1)
    (font-lock-mode -1))
  (advice-add 'kmacro-start-macro :before #'eli/speed-up-kmacro)

  (defun eli/speed-up-kmacro-recover ()
    (smartparens-mode 1)
    (add-hook 'post-self-insert-hook
              #'my/yas-try-expanding-auto-snippets)
    (winner-mode 1)
    (hungry-delete-mode 1)
    (aggressive-indent-mode 1)
    (flyspell-mode 1)
    (corfu-mode 1)
    (font-lock-mode 1))
  
  (advice-add 'kmacro-keyboard-quit :after #'eli/speed-up-kmacro-recover)
  (advice-add 'markmacro-exit :after #'eli/speed-up-kmacro-recover))


(with-eval-after-load 'rect
  (keymap-set rectangle-mark-mode-map "M-w" #'copy-rectangle-as-kill)
  (keymap-set rectangle-mark-mode-map "C-w" #'kill-rectangle)
  (keymap-set rectangle-mark-mode-map "C-d" #'delete-rectangle)
  (keymap-set rectangle-mark-mode-map "C-w" #'kill-rectangle)
  (keymap-set rectangle-mark-mode-map "M-d" #'delete-whitespace-rectangle))

(add-hook 'elemacs-first-file-hook #'pixel-scroll-precision-mode)
(provide 'init-better-defaults)
;;; init-better-defaults.el ends here.
