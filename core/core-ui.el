;; core-ui.el --- Initialize core-ui.el.	-*- lexical-binding: t; -*-

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


;;;; themes.
(load-theme 'mindre t)


;;;; modeline.
(setup mood-line
  (:hook-into after-init-hook)
  (:option*
   mood-line-glyph-alist  mood-line-glyphs-fira-code))


;;;; font
(defun eli/set-font ()
  (progn
    ;; Setting English Font
    (set-face-attribute 'default nil :font "Cascadia Mono 13")
	
    ;; Chinese fonts
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
			            (font-spec :family "Sarasa Mono SC Nerd")))

	;;; modeline font setting
	(create-fontset-from-fontset-spec
	 (font-xlfd-name
	  (font-spec :family "Cascadia Mono"
				 :registry "fontset-modeline fontset")))

	(dolist (charset '(kana han cjk-misc bopomofo unicode))
      (set-fontset-font "fontset-modeline fontset"  charset
			            (font-spec :family "InconsolataGo QiHei NF")))
	(set-fontset-font "fontset-modeline fontset" 'latin
					  (font-spec :family "Cascadia Mono"))
	
	(dolist (face '(mode-line mode-line-inactive mode-line-buffer-id
							  mode-line-highlight mode-line-active
							  mode-line-emphasis))
	  (set-face-attribute face nil :fontset "fontset-modeline fontset"))
	(dolist (sym '(?● ?■ ?◢))
	  (set-fontset-font "fontset-modeline fontset" sym
						(font-spec :family "Sarasa Mono SC Nerd" :size 13)))
	;;; modeline font setting ends here

	(dolist (font '("AR PL New Kai" "Meslo LG S DZ" "HanaMinA" "Noto Color Emoji"))
	  (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
	
    (set-fontset-font nil ?ㄓ (font-spec :family "Sarasa Mono SC Nerd" :size 12))
    (set-fontset-font nil ?⎙ "PragmataPro Liga")
	
    (setq face-font-rescale-alist '(("-cdac$" . 1.3)
                                    ("Sarasa" . 1.2)
                                    ("Amazon Ember" . 1.2)))))

(add-hook 'after-init-hook #'eli/set-font)



;;;; misc
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(setq x-gtk-use-system-tooltips nil)

;; Favor vertical splits over horizontal ones, since monitors are trending
;; toward wide rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)



(provide 'core-ui)
;;; core-ui.el ends here.
