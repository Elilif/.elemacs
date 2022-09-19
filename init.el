;; init.el -*- lexical-binding: t -*-

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

(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))

(setq user-emacs-directory "~/.emacs.d/")
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(setq custom-file (locate-user-emacs-file "custom.el"))

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(require 'init-benchmarking)
(require 'init-incremental-loading)
(require 'init-better-defaults)
(require 'init-ui)
(require 'init-minibuffer)
(require 'init-corfu)
(elemacs-load-packages-incrementally
 '(init-hydra))
(require 'init-org)
(require 'init-bib)
(require 'init-latex)
(require 'init-vc)
(require 'init-lang)
(require 'init-c)
(require 'init-lsp)
(require 'init-r)
(require 'init-completion)
(require 'init-flycheck)
(require 'init-reader)
(require 'init-news)
(require 'init-spell)
(require 'init-blog)
(require 'init-music)
(require 'init-finance)

;; fix keymap-set completing error
(defun keymap-set--anon-cmacro
    (form keymap &optional key definition)
  (ignore keymap key definition)
  (keymap--compile-check key)
  form)

(defun eli/open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f5>") 'eli/open-init-file)

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here.
