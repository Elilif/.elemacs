;; init-pdf.el --- Initialize init-pdf configurations.  -*- lexical-binding: t; -*-

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
  (require 'lib-org-noter)
  (require 'nov)
  (require 'lib-nov))

(setup pdf-tools
  (:iload pdf-tools)
  (:init
   (pdf-loader-install))
  (:also-load
   lib-pdf
   saveplace-pdf-view)
  (:option*
   pdf-view-use-scaling t
   pdf-view-use-imagemagick nil
   pdf-annot-activate-created-annotations t
   pdf-cache-image-limit 128
   pdf-cache-prefetch-delay 0.5)
  (:bind-into pdf-view-mode-map
    "v" +pdf-keyboard-select-region
    "O" pdf-occur)
  (:hooks pdf-tools-enabled-hook pdf-view-auto-slice-minor-mode
          pdf-tools-enabled-hook pdf-cache-prefetch-minor-mode
          pdf-tools-enabled-hook pdf-isearch-minor-mode))

;; (setup pdf-view-pagemark
;;   (:option*
;;    pdf-view-pagemark-timeout 0.5)
;;   (:hook-into pdf-view-mode-hook))

;; (setup org-noter
;;   (:iload org-noter)
;;   (:also-load
;;    lib-org-noter)
;;   (:hook org-pdftools-setup-link)
;;   (:option*
;;    org-noter-auto-save-last-location t
;;    org-noter-doc-split-fraction '(0.65 0.35)
;;    org-noter-notes-search-path '("~/Dropbox/org/roam")
;;    org-noter-always-create-frame nil)
;;   (:bind-into org-noter-notes-mode-map
;;     "M-]" eli/org-noter-scroll-up-other-window
;;     "M-[" eli/org-noter-scroll-down-other-window)
;;   (:bind-into org-noter-doc-mode-map
;;     "c" eli/org-noter-screenshot)
;;   (:hooks
;;    org-noter-insert-heading-hook eli/org-noter-set-highlight
;;    org-noter-insert-heading-hook (lambda () (delete-char -1)))
;;   (:advice
;;    org-noter--focus-notes-region :after eli/org-noter-set-highlight
;;    org-noter--insert-heading :override eli/org-noter--insert-heading
;;    org-noter-kill-session :before eli/org-noter-kill-outline))

(setup org-doc-noter
  (:option*
   org-doc-noter-doc-split-fraction 0.55)
  (:hooks
   org-doc-noter-insert-heading-hook (lambda(arg)
                                       (when arg
                                         (org-entry-put nil "NOANKI" "t"))))
  (:bind-into org-doc-noter-doc-mode-map
    "e" eli/org-doc-noter-insert-exercise
    "a" eli/org-doc-noter-insert-anki))

(setup nov
  (:iload nov)
  (:also-load
   lib-nov)
  (:init
   (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
  (:option*
   nov-text-width 80)
  (:bind
   "o" nov-goto-toc)
  (:advice
   nov-content-unique-identifier :override my-nov-content-unique-identifier))

(setup eww
  (:option*
   eww-retrieve-command '("readable")
   shr-use-xwidgets-for-media t))


;;;; provide
(provide 'init-pdf)
;;; init-pdf.el ends here.
