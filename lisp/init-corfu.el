;; init-corfu.el --- Initialize corfu configurations.	-*- lexical-binding: t -*-

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

(elemacs-require-package 'corfu)
(add-hook 'after-init-hook 'global-corfu-mode)
(with-eval-after-load 'corfu
  (setq corfu-cycle       t
	corfu-auto        t
	corfu-separator ?\s
	corfu-max-width 150
	corfu-auto-prefix 3
    corfu-excluded-modes '(org-mode)
	corfu-on-exact-match nil))

(elemacs-require-package 'kind-icon)
(with-eval-after-load 'corfu
  (setq kind-icon-use-icons t
	kind-icon-default-face 'corfu-default
	kind-icon-blend-background nil
	kind-icon-blend-frac 0.08)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(elemacs-require-package 'cape)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)
(provide 'init-corfu)
;;; init-corfu.el ends here.
