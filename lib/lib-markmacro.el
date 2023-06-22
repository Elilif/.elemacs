;; lib-markmacro.el --- Initialize lib-markmacro configurations.	-*- lexical-binding: t; -*-

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

(defun eli/speed-up-kmacro (_args)
  (winner-mode -1)
  (hungry-delete-mode -1)
  (aggressive-indent-mode -1)
  (corfu-mode -1)
  (font-lock-mode -1))

(defun eli/speed-up-kmacro-recover ()
  (winner-mode 1)
  (hungry-delete-mode 1)
  (when (and (derived-mode-p 'prog-mode)
			 (not (member major-mode '(makefile-gmake-mode))))
	(aggressive-indent-mode 1)
	(corfu-mode 1))
  (font-lock-mode 1))


;;;; provide
(provide 'lib-markmacro)
;;; lib-markmacro.el ends here.
