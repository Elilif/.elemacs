;; core-incremental-loading.el --- Initialize configurations.	-*- lexical-binding: t; -*-

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
;;  Loading packages incrementally. The code in this file is mainly taken from
;;  https://github.com/doomemacs/doomemacs/blob/e96624926/lisp/doom-start.el#L180
;;

;;; Code:


;;;; Customizations
(defgroup elemacs-iloader nil
  "Load packages incrementally."
  :group 'elemacs)

(defvar elemacs-incremental-packages '(t)
  "A list of packages to load incrementally after startup. Any large packages
  here may cause noticeable pauses, so it's recommended you break them up into
  sub-packages. For example, `org' is comprised of many packages, and can be
  broken up into:

    (elemacs-load-packages-incrementally
     \='(calendar find-func format-spec org-macs org-compat
       org-faces org-entities org-list org-pcomplete org-src
       org-footnote org-macro ob org org-clock org-agenda
       org-capture))

  Incremental loading does not occur in daemon sessions (they are
  loaded immediately at startup).")

(defcustom elemacs-incremental-first-idle-timer (if (daemonp) 0 2)
  "How long (in idle seconds) until incremental loading starts.

 Set this to nil to disable incremental loading. Set this to 0 to
load all incrementally deferred packages immediately at
`emacs-startup-hook'."
  :group 'elemacs-iloader
  :type 'number)

(defcustom elemacs-incremental-idle-timer 2
  "How long (in idle seconds) in between incrementally loading packages."
  :group 'elemacs-iloader
  :type 'number)


;;;; Loading packages incrementally.
(defun elemacs-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

If NOW is non-nil, load PACKAGES incrementally,
in `elemacs-incremental-idle-timer' intervals."
  (let ((gc-cons-threshold most-positive-fixnum))
    (if (not now)
        (cl-callf append elemacs-incremental-packages packages)
      (while packages
        (let ((req (pop packages)))
          (condition-case-unless-debug e
              (or (not
                   (while-no-input
                     ;; (message "Loading %s (%d left)" req (length packages))
                     ;; If `default-directory' doesn't exist or is
                     ;; unreadable, Emacs throws file errors.
                     (let ((default-directory user-emacs-directory)
                           (inhibit-message t)
                           (file-name-handler-alist
                            (list (rassq 'jka-compr-handler file-name-handler-alist))))
                       (require req nil t)
                       nil)))
                  (push req packages))
            (error
             (message "Error: failed to incrementally load %S because: %s" req e)
             (setq packages nil)))
          ;; (if (null packages)
          ;;     (message "Incrementally loading finished!")
          (when packages
            (run-with-idle-timer elemacs-incremental-idle-timer
								 nil #'elemacs-load-packages-incrementally
								 packages t)
            (setq packages nil)))))))

(defun elemacs-load-packages-incrementally-h ()
  "Begin incrementally loading packages in `elemacs-incremental-packages'.

If this is a daemon session, load them all immediately instead."
  (when (numberp elemacs-incremental-first-idle-timer)
    (if (zerop elemacs-incremental-first-idle-timer)
        (mapc #'require (cdr elemacs-incremental-packages))
      (run-with-idle-timer elemacs-incremental-first-idle-timer
                           nil #'elemacs-load-packages-incrementally
                           (cdr elemacs-incremental-packages) t))))

(add-hook 'emacs-startup-hook #'elemacs-load-packages-incrementally-h)

;; (elemacs-load-packages-incrementally
;;  '(borg init-hydra calendar find-func format-spec org-macs org-compat
;; 	    org-faces org-entities org-list org-pcomplete org-src
;; 	    org-footnote org-macro ob org org-clock org-agenda
;; 	    org-capture dired-x ispell all-the-icons mu4e embark emms-setup org-roam
;;         tex-site texmath))

;; ;; pdf
;; (elemacs-load-packages-incrementally
;;  '(image-mode pdf-macs pdf-util pdf-info pdf-cache jka-compr
;;               pdf-view pdf-annot pdf-tools pdf-occur org-noter org-refile))

;; ;; vc
;; (elemacs-load-packages-incrementally
;;  '(dash f s with-editor package eieio transient git-commit))


(provide 'core-incremental-loading)
;;; core-incremental-loading.el ends here.
