;; init-pdf.el --- Initialize init-pdf configurations.	-*- lexical-binding: t; -*-

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
  (require 'pdf-tools)
  (require 'org-noter)
  (require 'lib-pdf)
  (require 'lib-org-noter))

(setup pdf-tools
  (:init
   ;; (run-with-idle-timer 2 nil #'pdf-loader-install)
   (pdf-loader-install)
   )
  (:also-load
   lib-pdf
   saveplace-pdf-view)
  (:option*
   pdf-view-use-scaling t
   pdf-view-use-imagemagick nil
   pdf-annot-activate-created-annotations t)
  (:bind-into pdf-view-mode-map
	"v" +pdf-keyboard-select-region
	"O" pdf-occur)
  (:hooks pdf-tools-enabled-hook pdf-view-auto-slice-minor-mode
		  pdf-tools-enabled-hook pdf-isearch-minor-mode)
  )

(setup org-noter
  (:also-load
   lib-org-noter)
  (:hook org-pdftools-setup-link)
  (:option*
   org-noter-auto-save-last-location t
   org-noter-doc-split-fraction '(0.65 0.35)
   org-noter-notes-search-path '("~/Dropbox/org/roam")
   org-noter-always-create-frame t)
  (:bind-into org-noter-notes-mode-map
	"M-]" eli/org-noter-scroll-up-other-window
	"M-[" eli/org-noter-scroll-down-other-window)
  (:hooks org-noter-insert-heading-hook eli/org-noter-set-highlight)
  (:advice org-noter--focus-notes-region :after eli/org-noter-set-highlight
		   org-noter--insert-heading :override eli/org-noter--insert-heading))


;;;; provide
(provide 'init-pdf)
;;; init-pdf.el ends here.
