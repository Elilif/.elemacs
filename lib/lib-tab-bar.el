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

(require 'tabspaces)
(require 'burly)

(defvar emms-playing-time-mode)
(defvar emms-playing-time-string)
(defvar emms-lyrics-display-on-modeline)
(defvar emms-lyrics-mode-line-string)

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

(defface tar-bar-icons
  '((default
     :height 1.3 :inherit mindre-keyword))
  "Tab bar face for non-selected tab."
  :version "30.0.50"
  :group 'tab-bar-faces)

(defun eli/tab-bar-icon ()
  "Show `' in tab-bar."
  (propertize ""
              'face 'tar-bar-icons))

(defun eli/tab-bar-emms ()
  (concat
   (when (and (boundp 'emms-lyrics-display-on-modeline)
              emms-lyrics-display-on-modeline
              (not (string-empty-p emms-lyrics-mode-line-string)))
     (format-mode-line emms-lyrics-mode-line-string 'mood-line-unimportant))
   (when (and (boundp 'emms-playing-time-mode)
              emms-playing-time-mode
              (not (string-empty-p emms-playing-time-string)))
     (format-mode-line emms-playing-time-string 'mood-line-unimportant))))

(defface tab-bar-svg-active
  '((t (:family "Cascadia Mono" :foreground "#a1aeb5"
                :box (:line-width (4 . 5)
                                  :color "#a1aeb5"
                                  :style flat-button))))
  "Tab bar face for selected tab.")

(defvar eli/tab-bar-svg-padding-cache nil)

(defun eli/tab-bar-svg-padding (width string)
  (unless eli/tab-bar-svg-padding-cache
    (setq eli/tab-bar-svg-padding-cache (make-hash-table :test #'equal)))
  (with-memoization (gethash (list width string)
                             eli/tab-bar-svg-padding-cache)
    (let* ((margin (plist-get svg-lib-style-default :margin))
           (txt-char-width  (window-font-width nil 'fixed-pitch))
           (tag-width (- width (* margin txt-char-width)))
           (padding (- (/ tag-width txt-char-width) (length string))))
      padding)))

(defun eli/tab-bar-tab-name-with-svg (tab i)
  (let* ((current-p (eq (car tab) 'current-tab))
         (width (/ (* (/ (frame-inner-width) 3) 2)
                   (length (funcall #'tab-bar-tabs))))
         (name (concat (if tab-bar-tab-hints (format "%d " i) "")
                       (alist-get 'name tab)
                       (or (and tab-bar-close-button-show
                                (not (eq tab-bar-close-button-show
                                         (if current-p 'non-selected 'selected)))
                                tab-bar-close-button)
                           ""))))
    (when tab-bar-auto-width-min
      (setq width (max width (if (window-system)
                                 (nth 0 tab-bar-auto-width-min)
                               (nth 1 tab-bar-auto-width-min)))))
    (when tab-bar-auto-width-max
      (setq width (min width (if (window-system)
                                 (nth 0 tab-bar-auto-width-max)
                               (nth 1 tab-bar-auto-width-max)))))
    (let ((continue t)
          (prev-width (string-pixel-width name))
          (padding (plist-get svg-lib-style-default :padding))
          curr-width)
      (cond
       ((< prev-width width)
        (setq padding (eli/tab-bar-svg-padding width name)))
       ((> prev-width width)
        (while continue
          (setf (substring name -1) "")
          (setq curr-width (string-pixel-width name))
          (if (and (> curr-width width)
                   (< curr-width prev-width))
              (setq prev-width curr-width)
            (setq continue nil)))))
      (propertize
       name
       'face 'tab-bar-tab-inactive
       'display
       (svg-tag-make
        name
        :face 'tab-bar-svg-active
        :inverse (eq (car tab) 'current-tab) :margin 0 :radius 6 :padding padding
        :height 1.3 :ascent 17 :scale 1.0)))))

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
(defun eli/tabspaces-restore-one-session ()
  "Select one workspace and restore it."
  (interactive)
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
  (when tab-bar-mode
    (let ((curr (tab-bar--current-tab-index)))
      (tab-bar-select-tab 1)
      (when (<= (length (tabspaces--buffer-list)) 2)
        (tab-bar-close-tab 1))
      (tab-bar-select-tab (+ curr 1)))))

;;;###autoload
(defun eli/tabspaces-save-session ()
  "Save tabspace name and buffers."
  (interactive)
  ;; Start from an empty list.
  (setq tabspaces--session-list nil)
  (let ((curr (tab-bar--current-tab-index)))
    ;; loop over tabs
    (cl-loop for tab in (tabspaces--list-tabspaces)
             do (progn
                  (tab-bar-select-tab-by-name tab)
                  (when-let ((files  (tabspaces--store-buffers (tabspaces--buffer-list))))
                    (setq tabspaces--session-list
                          (append tabspaces--session-list
                                  (list (cons tab (cons
                                                   files
                                                   (window-state-get nil 'writable)))))))
                  (when (and (featurep 'burly)
                             (member tab (burly-bookmark-names)))
                    (eli/burly-bookmark-windows tab))))
    ;; As tab-bar-select-tab starts counting from 1, we need to add 1 to the index.
    (tab-bar-select-tab (+ curr 1)))
  ;; Write to file
  (with-temp-file tabspaces-session-file
    (point-min)
    (insert ";; -*- mode: emacs-lisp; lexical-binding:t; coding: utf-8-emacs; -*-\n"
            tabspaces-session-header
            ";; Created " (current-time-string) "\n\n"
            ";; Tabs and buffers:\n")
    (insert "(setq tabspaces--session-list '" (format "%S" tabspaces--session-list) ")")))

;;;###autoload
(defun eli/tabspaces-restore-session (&optional session)
  "Restore tabspaces session."
  (interactive)
  (load-file (or session
                 tabspaces-session-file))
  ;; Start looping through the session list, but ensure to start from a
  ;; temporary buffer "*tabspaces--placeholder*" in order not to pollute the
  ;; buffer list with the final buffer from the previous tab.
  (cl-loop for elm in tabspaces--session-list do
           (tabspaces-switch-or-create-workspace (car elm))
           (switch-to-buffer "*tabspaces--placeholder*")
           (mapc #'find-file (cadr elm))
           (if (member (car elm) (burly-bookmark-names))
               (burly-open-bookmark (car elm))
             (window-state-put (cddr elm))))
  ;; Once the session list is restored, remove the temporary buffer from the
  ;; buffer list.
  (cl-loop for _elm in tabspaces--session-list do
           (tabspaces-remove-selected-buffer "*tabspaces--placeholder*"))
  ;; Finally, kill the temporary buffer to clean up.
  (kill-buffer "*tabspaces--placeholder*"))

;;;; provide
(provide 'lib-tab-bar)
;;; lib-tab-bar.el ends here.
