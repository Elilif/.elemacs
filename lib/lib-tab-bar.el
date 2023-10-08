;; lib-tab-bar.el --- Initialize lib-tab-bar configurations.    -*- lexical-binding: t; -*-

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

;;;; tab-bar
(defface tab-bar-hints
  '((default
     :height 0.8 :inherit tab-bar-tab-inactive))
  "Tab bar face for non-selected tab."
  :version "30.0.50"
  :group 'tab-bar-faces)

(defface tab-bar-tab-space-inactive
  '((default
     :height 0.8 :inherit tab-bar-tab-inactive))
  "Tab bar face for non-selected tab."
  :version "30.0.50"
  :group 'tab-bar-faces)

(defface tab-bar-tab-space-active
  '((default
     :height 0.8 :inherit tab-bar-tab))
  "Tab bar face for non-selected tab."
  :version "30.0.50"
  :group 'tab-bar-faces)

;;;; tabspaces
(defvar consult--source-workspace
  (list :name     "Workspace Buffers"
        :narrow   ?w
        :history  'buffer-name-history
        :category 'buffer
        :state    #'consult--buffer-state
        :default  t
        :items    (lambda () (consult--buffer-query
                              :predicate #'tabspaces--local-buffer-p
                              :sort 'visibility
                              :as #'buffer-name)))

  "Set workspace buffer list for consult-buffer.")

(defun my--consult-tabspaces ()
  "Deactivate isolated buffers when not using tabspaces."
  (cond (tabspaces-mode
         ;; hide full buffer list (still available with "b")
         (consult-customize consult--source-buffer :hidden t :default nil)
         (add-to-list 'consult-buffer-sources 'consult--source-workspace))
        (t
         ;; reset consult-buffer to show all buffers
         (consult-customize consult--source-buffer :hidden nil :default t)
         (setq consult-buffer-sources (remove 'consult--source-workspace consult-buffer-sources)))))

(defvar eli/tabspaces-kill-buffer-exclude '("*scratch*"
                                            "*org-scratch*"
                                            "*lisp-interaction-scratch*"))

;;;###autoload
(defun eli/tabspaces-kill-buffers-close-workspace ()
  "Kill all buffers in the workspace and then close the workspace itself."
  (interactive)
  (if current-prefix-arg
      (let ((buf (tabspaces--buffer-list)))
        (unwind-protect
            (cl-loop for b in buf
                     do (unless (member (buffer-name b)
                                        eli/tabspaces-kill-buffer-exclude)
                          (kill-buffer b)))
          (tab-bar-close-tab)))
    (tab-bar-close-tab)))

;;;###autoload
(defun eli/tabspaces-restore-session ()
  "Select one workspace and restore it."
  (interactive)
  (require 'tabspaces)
  (require 'burly)
  (load-file tabspaces-session-file)
  (let* ((name (completing-read "Select a session: " tabspaces--session-list))
         (session (assoc name tabspaces--session-list)))
    (tabspaces-switch-or-create-workspace (car session))
    (switch-to-buffer "*tabspaces--placeholder*")
    (mapc #'find-file (cadr session))
    (if (member (car session) (burly-bookmark-names))
        (burly-open-bookmark (car session))
      (window-state-put (cddr session)))
    (tabspaces-remove-selected-buffer "*tabspaces--placeholder*")
    (kill-buffer "*tabspaces--placeholder*")
    (eli/tabspaces-delete-empty-tab)))

(defun eli/tabspaces-delete-empty-tab (&rest _args)
  "Delete the first empty workspace."
  (let ((curr (tab-bar--current-tab-index)))
    (tab-bar-select-tab 1)
    (when (<= (length (tabspaces--buffer-list)) 2)
      (tab-bar-close-tab 1))
    (tab-bar-select-tab (+ curr 1))))

;;;; provide
(provide 'lib-tab-bar)
;;; lib-tab-bar.el ends here.
