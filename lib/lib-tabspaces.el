;; lib-tabspaces.el --- Initialize lib-tabspaces configurations.	-*- lexical-binding: t; -*-

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


;;;; provide
(provide 'lib-tabspaces)
;;; lib-tabspaces.el ends here.
