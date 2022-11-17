;; init-c.el --- Initialize c configurations.	-*- lexical-binding: t -*-

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

(with-eval-after-load 'cc-mode
  (defun eli/cc-mode-hook ()
    (let* ((file-name (buffer-file-name))
	       (is-windows (equal 'windows-nt system-type))
	       (exec-suffix (if is-windows ".exe" ".out"))
	       (os-sep (if is-windows "\\" "/")))
      (if file-name
	      (progn
	        (setq file-name (file-name-nondirectory file-name))
	        (let ((out-file (concat (file-name-sans-extension file-name) exec-suffix)))
	          (setq-local compile-command (format "g++ -std=c++11 -g %s -o %s" file-name out-file)))))))
  (setq-default c-basic-offset 4)
  (add-hook 'c-mode-common-hook (lambda () (c-set-style "stroustrup")))
  (add-hook 'c-mode-common-hook #'eli/cc-mode-hook)
  (keymap-set c-mode-base-map "(" nil)
  (keymap-set c-mode-base-map "{" nil)
  (keymap-set c-mode-base-map "C-c C-o" #'ff-find-other-file))

(add-hook 'c-mode-common-hook #'modern-c++-font-lock-global-mode)
(add-hook 'c-mode-hook (lambda () (require 'ccls)))
(add-hook 'c++-mode-hook (lambda () (require 'ccls)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(with-eval-after-load 'gdb-mi
  (setq gdb-show-main t
        gdb-many-windows t)
  
  (defun gdb-non-stop-handler ()
    (goto-char (point-min))
    (if (re-search-forward "No symbol" nil t)
        (progn
	      (message
           "This version of GDB doesn't support non-stop mode.  Turning it off.")
	      (setq gdb-non-stop nil)
	      (setq gdb-supports-non-stop nil))
      (setq gdb-supports-non-stop t)
      (gdb-input "-gdb-set mi-async 1" 'ignore)
      (gdb-input "-list-target-features" 'gdb-check-target-async))))



(provide 'init-c)
;;; init-c.el ends here.
