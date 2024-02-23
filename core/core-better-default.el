;; core-better-default.el --- Initialize core-better-default configurations.    -*- lexical-binding: t; -*-

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
  (require 'rect)
  (require 'midnight)
  (require 'ibuf-ext)
  (require 'dired)
  (require 'core-lib))


;;;; misc
;; enable recursive minibuffer
(setq enable-recursive-minibuffers t)

;; accept function redefinition warning
(setq ad-redefinition-action 'accept)

;; disable large file warning
(setq large-file-warning-threshold nil)

;; Tab.space equivalence
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; pdf cache setting
(setq image-cache-eviction-delay 60)

(setq save-interprogram-paste-before-kill t)

(setq copy-region-blink-delay 0)

(setq word-wrap-by-category t)

(setq help-at-pt-display-when-idle t)

(setq sentence-end-double-space nil)

(setq bookmark-fringe-mark nil)

;; set fill column
(setq-default fill-column 80)

;; use system trash
(setq delete-by-moving-to-trash t)

(setq custom-buffer-done-kill t)

(setq switch-to-buffer-obey-display-actions t)

(setup autorevert
  (:once (list :hooks 'find-file-hook)
    (:option global-auto-revert-non-file-buffers t)
    (global-auto-revert-mode 1)))

(setq set-mark-command-repeat-pop t)

(setq make-backup-files nil)
(setq auto-save-default t
      auto-save-interval 200
      auto-save-timeout 20)
(setq create-lockfiles t)

(setq initial-frame-alist '((fullscreen . maximized)))

(setup delsel
  (:once (list :hooks 'pre-command-hook)
    (delete-selection-mode 1)))

(setup hl-line
  (:once (list :hooks 'find-file-hook)
    (:once (list :hooks 'pre-command-hook)
      (global-hl-line-mode 1))))

(setup frame
  (:once (list :before 'self-insert-command)
    (:option blink-cursor-mode nil)))

(setq auto-save-list-file-prefix nil)

;; use proxy
(setq url-proxy-services '(("http" . "127.0.0.1:7890")
                           ("https" . "127.0.0.1:7890")
                           ;; ("socks5" . "127.0.0.1:7891")
                           ))

(setq use-short-answers t)

(setq save-silently t)

(setq custom-file null-device)
(add-to-list 'exec-path "~/.local/bin/")

(keymap-global-set "C-c C-k" #'kill-buffer-and-window)

(setq disabled-command-function nil)

;; (setq scroll-conservatively 100)

;;;; hippie-expand
(setup hippie-exp
  (:when-loaded
    (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                             try-expand-dabbrev-all-buffers
                                             try-expand-dabbrev-from-kill
                                             try-complete-file-name-partially
                                             try-complete-file-name
                                             try-expand-all-abbrevs
                                             try-expand-list
                                             try-expand-line
                                             try-complete-lisp-symbol-partially
                                             try-complete-lisp-symbol)))
  (:global "s-/" hippie-expand)
  (:with-feature dabbrev
    (:option*
     dabbrev-ignored-buffer-modes '(archive-mode image-mode pdf-view-mode))))


;;;; show minibuffer depth
(setup mb-depth
  (:once (list :hooks 'minibuffer-setup-hook)
    (minibuffer-depth-indicate-mode 1)))


;;;; occur
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

(setup replace
  (:global "M-s o" occur-dwim)
  (:with-hook occur-hook
    (:hook (lambda ()
             (switch-to-buffer-other-window "*Occur*"))))
  (:with-mode occur-mode
    (:bind "q" kill-this-buffer)))

;;;; use winner-mode
(defun transient-winner-undo ()
  "Transient version of winner-undo."
  (interactive)
  (let ((echo-keystrokes nil)
        (back (lambda ()
                (interactive)
                (if tab-bar-mode
                    (tab-bar-history-back)
                  (winner-undo))))
        (forward (lambda ()
                   (interactive)
                   (if tab-bar-mode
                       (tab-bar-history-forward)
                     (winner-redo)))))
    (funcall back)
    (message "Winner: [u]ndo [r]edo")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map [?u] back)
       (define-key map [?r] forward)
       map)
     t)))

;; SRC:
;; https://github.com/ksqsf/emacs-config/blob/master/modules/prelude-ui.el#L419
(defun window-lift ()
  "Lift the selected window to replace its parent in the window tree."
  (interactive)
  (let ((sibling (or (window-prev-sibling)
                     (window-next-sibling)))
        (keymap (let ((keymap (make-sparse-keymap)))
                  (define-key keymap (kbd "l") #'window-lift)
                  keymap)))
    ;; In the following configuration
    ;;
    ;; |-----|-----|
    ;; | W1  |     |
    ;; |-----| W3  |
    ;; | W2  |     |
    ;; |-----|-----|
    ;;
    ;; If W3 is selected, it's prev-sibling won't be a leaf.
    (when (and sibling
               (not (and (window-buffer sibling)
                         (minibufferp (window-buffer sibling)))))
      (set-transient-map keymap t nil nil 0.5)
      (delete-window sibling))))

(setup winner
  (:once (list :hooks 'pre-command-hook)
    (winner-mode 1))
  (:global "C-c u" transient-winner-undo))


;;;; dired
(setup dired
  (:bind "q" kill-this-buffer
         "RET" dired-find-alternate-file)
  (:also-load dired-x)
  (:when-loaded
    (:option dired-listing-switches "-alh"
             dired-recursive-copies 'always
             dired-recursive-deletes 'always
             ;; 在Bookmark中进入dired buffer时自动刷新
             dired-auto-revert-buffer t
             dired-dwim-target t
             dired-kill-when-opening-new-dired-buffer t
             dired-guess-shell-alist-user '(("\\.doc\\'" "wps")
                                            ("\\.docx\\'" "wps")))
    (put 'dired-find-alternate-file 'disabled nil))
  (:global
   [remap find-file] eli/find-file-or-create))

;;;; recentf
(setup recentf
  (:iload recentf)
  (:when-loaded
    (:option recentf-auto-cleanup 'never
             recentf-exclude '("/home/eli/.emacs.d/.cache/treemacs-persist-at-last-error"
                               "/home/eli/.emacs.d/.cache/treemacs-persist"
                               "\\.txt$"
                               "\\.chat$"
                               "/home/eli/.emacs.d/emms/history"
                               "/home/eli/.emacs.d/elpa/*"
                               "/home/eli/.elfeed/index"
                               "~/.elfeed/index"
                               "/home/eli/.mail/*"
                               "/tmp/*")
             recentf-max-menu-items 50
             recentf-max-saved-items 50
             recentf-save-file "~/.emacs.d/var/recentf")))

;;;; saveplace
(setup saveplace
  (:once (list :hooks 'pre-command-hook)
    (save-place-mode 1))
  (:init
   (setq save-place-file "~/.emacs.d/var/places")))

;;;; ibuffer
(setup ibuffer
  (:global [remap list-buffers] ibuffer)
  (:option*
   ibuffer-formats '((mark modified read-only locked " "
                           (name 30 30 :left :elide)
                           " "
                           (size 9 -1 :right)
                           " "
                           (mode 16 16 :left :elide)
                           " " filename-and-process)
                     (mark " "
                           (name 16 -1)
                           " " filename))
   ibuffer-saved-filter-groups
   '(("default"
      ("dired" (mode . dired-mode))
      ("emacs" (or
                (mode . emacs-lisp-mode)
                (name . ".*scratch.*")
                (name . "^\\*Messages\\*$")))
      ("magit" (or
                (mode . magit-status-mode)
                (mode . magit-process-mode)
                (mode . magit-diff-mode)
                (mode . magit-revision-mode)
                (mode . magit-log-mode)))
      ("pdf" (or
              (file-extension . "pdf")
              (mode . pdf-outline-buffer-mode)))
      ("roam"  (or
                (filename . "/home/eli/Dropbox/org/roam/main/")
                (filename . "/home/eli/Dropbox/org/roam/references/")))
      ("books" (filename . "/home/eli/Dropbox/org/roam/books/"))
      ("agenda" (or
                 (filename . "/home/eli/Dropbox/org/")
                 (name . "^\\*Org Agenda\\*$"))))))
  (:hook (lambda ()
           (ibuffer-switch-to-saved-filter-groups "default"))))

;;;; rect
(setup rect
  (:with-map rectangle-mark-mode-map
    (:bind "M-w" copy-rectangle-as-kill
           "C-w" kill-rectangle
           "C-d" delete-rectangle
           "M-d" delete-whitespace-rectangle))
  (:global "C-r" 'rectangle-mark-mode))

;;;; pixel-scroll
(setup pixel-scroll
  (:once (list :hooks 'find-file-hook)
    (pixel-scroll-precision-mode))
  (setq scroll-preserve-screen-position 'always))

;;;; bookmark
(setup bookmark
  (:option*
   bookmark-default-file "~/.emacs.d/var/bookmarks"))

;;;; Eshell
(setup esh-mode
  (:option*
   eshell-directory-name "~/.emacs.d/var/eshell/"))

(setup url
  (:option*
   url-configuration-directory "~/.emacs.d/var/url/"))
;;;; bytecompile
(setup bytecomp
  (:with-map emacs-lisp-compilation-mode-map
    (:bind
     "q" kill-buffer-and-window)))
;;;; abbrev
(setup abbrev
  (:hook-into
   minibuffer-setup-hook))

(setup face-remap
  (:advice
   text-scale-mode :after #'eli/image-scale))

;;;; whitespace
(setup whitespace
  (:hook-into
   prog-mode-hook)
  (:option*
   whitespace-space-regexp "\\( +\\|\u200b\\)"
   whitespace-style '(face tabs spaces trailing space-before-tab newline
                           indentation empty space-after-tab space-mark
                           tab-mark)
   whitespace-display-mappings `((space-mark #x200b [?_])
                                 ,@whitespace-display-mappings)))

(setup paren
  (:when-loaded
    (defun eli/show-paren-function (fn)
      "Highlight enclosing parens."
      (cond ((looking-at-p "\\s(") (funcall fn))
            (t (save-excursion
                 (ignore-errors (backward-up-list))
                 (funcall fn))))))
  (:advice
   show-paren-function :around eli/show-paren-function))

;;;; midnight
(setup midnight
  ;; (once (list :before 'eli/consult-buffer)
  ;;   (require 'midnight)
  ;;   (advice-add 'clean-buffer-list :around #'suppress-messages)
  ;;   (run-with-idle-timer 30 t #'clean-buffer-list))
  ;; (:when-loaded
  ;;   (:option clean-buffer-list-delay-general 0.1
  ;;            clean-buffer-list-delay-special 1800
  ;;            clean-buffer-list-kill-buffer-names
  ;;            (append clean-buffer-list-kill-buffer-names
  ;;                    '("*elfeed-log*" "*Backtrace*"))
  ;;            clean-buffer-list-kill-regexps
  ;;            (append clean-buffer-list-kill-regexps
  ;;                    '("\\*Outline.*\\*" "\\*.*Profiler-Report.*\\*"))))
  (:once (list :hooks 'pre-command-hook)
    (midnight-mode)))

(provide 'core-better-default)
;;; core-better-default.el ends here.
