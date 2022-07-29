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
(elemacs-require-package 'doom-themes)
(load-theme 'doom-one-light t)
(setq doom-one-light-brighter-comments t)
(setq doom-one-light-brighter-modeline t)
(setq doom-one-light-padded-modeline nil)


;;; modeline.
(elemacs-require-package 'doom-modeline)
(add-hook 'after-init-hook 'doom-modeline-mode)
(setq doom-modeline-buffer-encoding t)
(column-number-mode 1)


;;; programing.
(elemacs-require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(defun eli/set-font ()
  (progn
    ;; Setting English Font
    (set-face-attribute 'default nil :font "Source Code Pro 13")
    ;; chinese fonts
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
			(font-spec :family "Sarasa Mono SC Nerd" :size 20)))
    (set-fontset-font "fontset-default" 'unicode "AR PL New Kai" nil 'prepend)
    (set-fontset-font "fontset-default" 'unicode "Noto Color Emoji" nil 'prepend)))
(if window-system
    (eli/set-font))

(provide 'init-ui)
;;; init-ui.el ends here.
