;; init-r.el --- Initialize r configurations.	-*- lexical-binding: t -*-

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

(elemacs-require-package 'ess)
(with-eval-after-load 'ess
  (defun eli/disable-corfu-auto ()
    (setq-local corfu-auto nil))
  (setq ess-use-flymake nil
	ess-ask-for-ess-directory nil
	ess-history-file "~/.R/.history"
	ess-tab-complete-in-script t
	comint-prompt-read-only t
	ess-use-eldoc 'script-only
	ess-use-company 'script-only
	)
  (add-hook 'inferior-ess-r-mode-hook #'eli/disable-corfu-auto))

(elemacs-require-package 'ess-smart-equals)
(with-eval-after-load 'ess
  (setq ess-smart-equals-extra-ops '(brace paren percent))
  (ess-smart-equals-activate))

(provide 'init-r)
;;; init-r.el ends here.
