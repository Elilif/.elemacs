;; init-flycheck.el --- Initialize flycheck configurations.	-*- lexical-binding: t -*-

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

(add-hook 'elemacs-first-buffer-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (setq flycheck-global-modes
	'(not text-mode outline-mode fundamental-mode lisp-interaction-mode emacs-lisp-mode
          org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode))
  (setq flycheck-idle-change-delay 3
        flycheck-idle-buffer-switch-delay 3
        flycheck-display-errors-delay 3))

(add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)



(provide 'init-flycheck)
;;; init-flycheck.el ends here.
