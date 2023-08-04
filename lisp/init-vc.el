;; init-vc.el<elemacs> --- Initialize vc configurations.    -*- lexical-binding: t -*-

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
  (require 'org-protocol)
  (require 'magit)
  (require 'lib-magit)
  (require 'vc-git))


;;;; magit
(setup magit
  (:when-loaded
    (require 'lib-magit)
    (:hooks
     git-rebase-mode-hook eli/magit-reverse-rebase-commits))
  (:iload magit)
  (:once (list :before 'magit-auto-revert-mode--init-kludge)
    (:option magit-no-message '("Turning on magit-auto-revert-mode...")))
  (:option*
   magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
   magit-clone-default-directory (expand-file-name
                                  (expand-file-name
                                   "src/Clone/"
                                   (getenv "HOME")))
   magit-clone-set-remote.pushDefault t
   magit-status-margin '(t age magit-log-margin-width nil 18)
   git-commit-summary-max-length      50
   git-commit-fill-column             72
   git-commit-finish-query-functions '(my-git-commit-check-style-conventions
                                       git-commit-check-style-conventions))
  (:bind-into magit-status-mode-map
    "q" mu-magit-kill-buffers
    "C" eli/magit-commit-add-log)
  (:bind-into magit-diff-section-map
    "C" eli/magit-commit-add-log))

(setup magit-todos
  (:once (list :before 'magit-status)
    (magit-todos-mode))
  (:option*
   magit-todos-auto-group-items 3
   magit-todos-branch-list nil)
  (:advice
   magit-todos--insert-todos :around eli/magit-insert))

;;;; forge
(setup forge
  (:iload forge)
  (:with-feature magit
    (:also-load forge))
  (:when-loaded
    (require 'lib-forge))
  (:init
   (setq forge-bug-reference-hooks '(forge-post-mode-hook
                                     ;; git-commit-setup-hook
                                     ;; magit-mode-hook
                                     )))
  (:option* forge-owned-accounts '(("eli" . (remote-name "personal")))
            forge-database-file "~/.emacs.d/var/forge/forge-database.sqlite")
  (:advice
   forge-insert-pullreqs :around eli/magit-insert
   forge-insert-issues :around eli/magit-insert))


;;;; provide
(provide 'init-vc)
;;; init-vc.el<elemacs> ends here.
