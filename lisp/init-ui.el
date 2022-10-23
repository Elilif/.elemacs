;; init-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-

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


;;; themes.
(load-theme 'mindre t)

;;; posframe

;;; modeline.
(add-to-list 'load-path "~/.emacs.d/site-lisp/mood-line/")
(autoload 'mood-line-mode "mood-line")
(add-hook 'after-init-hook 'mood-line-mode)
(column-number-mode 1)

;;; programing.
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(defun eli/set-font ()
  (progn
    ;; Setting English Font
    ;; (set-face-attribute 'default nil :font "Source Code Pro 13")
    (set-face-attribute 'default nil :font "Cascadia Mono 13")
    ;; Chinese fonts
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
			            (font-spec :family "Sarasa Mono SC Nerd")))
    (set-fontset-font "fontset-default" 'unicode "AR PL New Kai" nil 'prepend)
    (set-fontset-font "fontset-default" 'unicode "Meslo LG S DZ" nil 'prepend)
    (set-fontset-font "fontset-default" 'unicode "HanaMinA" nil 'prepend)
    (set-fontset-font "fontset-default" 'unicode "Noto Color Emoji" nil 'prepend)
    ;; fix the size of "ㄓ" to fit mode-line
    (set-fontset-font nil ?ㄓ (font-spec :family "Sarasa Mono SC Nerd" :size 12))
    (set-fontset-font nil ?⎙ "PragmataPro Liga")
    (setq face-font-rescale-alist '(("-cdac$" . 1.3)
                                    ("Sarasa" . 1.2)
                                    ("Amazon Ember" . 1.2)))))

(add-hook 'after-init-hook #'eli/set-font)

(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(setq x-gtk-use-system-tooltips nil)

;; Favor vertical splits over horizontal ones, since monitors are trending
;; toward wide rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)

(provide 'init-ui)
;;; init-ui.el ends here.
