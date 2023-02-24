;; init-info.el --- Initialize info configurations.	-*- lexical-binding: t -*-

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
  (require 'helpful)
  (require 'lib-info))

;;;; Helpful
(setup helpful
  (:when-loaded
	(require 'lib-helpful))
  (:global [remap describe-function] helpful-callable
           [remap describe-variable] helpful-variable
           [remap describe-key]      helpful-key
           [remap describe-symbol]   helpful-symbol)
  (:bind "q" kill-buffer-and-window)
  (:option* helpful-max-buffers 2
            helpful-switch-buffer-function #'+helpful-switch-to-buffer)
  (:advice helpful--update-and-switch-buffer :after  helpful-set-arguments-face
		   ;; remove `##' from functions list
		   helpful--callable-at-point :before  (lambda (&rest _arg) (aset obarray 0 nil))))

;;;; Info
(setup Info
  (:when-loaded
	(require 'lib-info))
  (:hook variable-pitch-mode)
  (:bind "C" eli/Info-copy-node-url)
  (:with-feature apropos
    (:hook (lambda ()
             (switch-to-buffer-other-window "*Apropos*")))))



(provide 'init-info)
;;; init-info.el ends here.
