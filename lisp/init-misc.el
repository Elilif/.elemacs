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
  (require 'lib-rime))

(setup dired
  (:once (list :packages 'dired)
    (diredfl-global-mode))
  (:hook all-the-icons-dired-mode)
  (:option* all-the-icons-dired-monochrome nil))

(setup ibuffer
  (:hook all-the-icons-ibuffer-mode)
  (:option* all-the-icons-ibuffer-icon t
            all-the-icons-ibuffer-color-icon t))

(setup hl-todo
  (:once (list :hooks 'find-file-hook)
    (global-hl-todo-mode)))

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
  (:option* popper-reference-buffers '("\\*Messages\\*"
                                       "\\*scratch\\*"
                                       "Output\\*$"
                                       "\\*Async Shell Command\\*"
                                       "\\*Xenops-Doctor\\*"
                                       "\\*Emms\\*.*"
                                       "\\*Org LATEX Export\\*"
                                       emms-browser-mode
                                       org-agenda-mode
                                       helpful-mode
                                       compilation-mode)
            popper-mode-line t
            popper-echo-dispatch-keys '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
            popper-display-control nil)
  (:advice popper-raise-popup :after (lambda (&optional _arg)
                                       (delete-other-windows))))

(setup shackle
  (:once (list :hooks 'find-file-hook)
    (shackle-mode))
  (:option* shackle-rules '(("*Messages*" :align below :size 0.3 :select t)
			                ("*scratch*" :select t :align right)
			                (helpful-mode :select t :align right)
			                (elfeed-show-mode :select t :align bottom :size 0.85)
			                ("\\*Outline.*\\*" :regexp t :align right :select t :size 0.3)
			                ("*WordNut*" :select t :align right :size 0.4)
			                ("\\*Emms\\*.*" :regexp t:align right :select t :size 0.5)
			                (emms-browser-mode :select t :align right :size 0.5)
			                (org-agenda-mode :select t)
			                ("*Org Select*" :select t :align right :size 0.3))))

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

(setup rime
  (:when-loaded
	(require 'lib-rime))
  (:once (list :hooks 'post-self-insert-hook)
	(:option default-input-method "rime"))
  (:option*
   rime-user-data-dir "~/.emacs.d/rime"
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

(provide 'init-misc)
;;; init-misc.el ends here.
