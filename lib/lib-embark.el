;; lib-embark.el --- Initialize lib-embark configurations.  -*- lexical-binding: t; -*-

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

;; preview image while using `find-file'
(defun eli/image-preview (&rest _args)
  (let* ((target (embark--targets))
         (file-path (plist-get (car target) :target))
         (name (file-name-nondirectory file-path))
         (mode (assoc-default name auto-mode-alist #'string-match)))
    (posframe-hide-all)
    (when (memq mode '(image-mode))
      (with-current-buffer (get-buffer-create "*image*")
        (setq inhibit-read-only t)
        (erase-buffer)
        (insert-file-contents file-path)
        (set-auto-mode-0 mode))
      (when (posframe-workable-p)
        (posframe-show "*image*"
                       :poshandler #'posframe-poshandler-frame-center)))))

;;;###autoload
(defun eli/select-images ()
  (interactive)
  (read-file-name "Select a file: "
                  "~/Documents/org-images/"))

(advice-add 'eli/select-images :before (lambda (&rest _args)
                                         (letrec ((eli/remove-preview (lambda ()
                                                                        (remove-hook 'post-command-hook #'eli/image-preview)
                                                                        (posframe--kill-buffer "*image*")
                                                                        (remove-hook 'minibuffer-exit-hook eli/remove-preview))))
                                           (add-hook 'post-command-hook #'eli/image-preview)
                                           (add-hook 'minibuffer-exit-hook eli/remove-preview))))


(defvar-keymap embark-org-roam-map
  "i" #'org-roam-node-insert
  "s" #'embark-collect
  "b" #'eli/org-roam-backlinks-node-read)

(defvar-keymap embark-consult-org-headline-map
  "i" #'embark-insert
  "b" #'consult-org-headline-insert-backlink
  "r" #'consult-org-headline-insert-reference
  "w" #'embark-copy-as-kill
  "q" #'embark-toggle-quit
  "E" #'embark-export
  "S" #'embark-collect
  "L" #'embark-live
  "B" #'embark-become
  "A" #'embark-act-all
  "C-s" #'embark-isearch
  "SPC" #'mark
  "DEL" #'delete-region)

;;;###autoload
(defun eli/embark-deselect ()
  "Deselect all selected candidates."
  (interactive)
  (when embark--selection
    (dolist (target embark--selection)
      (when-let ((overlay (cdr target)))
        (delete-overlay overlay)))
    (setq embark--selection nil)))

;;; file manipulations
(defun eli/common-parent (paths)
  "Return the deepest common parent directory of PATHS."
  (if (cdr paths)
      (when paths
        (let ((common-prefix (car paths)))
          (dolist (path (cdr paths))
            (let ((components1 (file-name-split common-prefix))
                  (components2 (file-name-split path)))
              (setq common-prefix "")
              (while (and components1 components2
                          (string= (car components1) (car components2)))
                (setq common-prefix (file-name-concat common-prefix  (car components1)))
                (setq components1 (cdr components1))
                (setq components2 (cdr components2)))))
          (expand-file-name (file-name-as-directory common-prefix))))
    (file-name-parent-directory (car paths))))

(defun directory-same-p (lst)
  "Return t if all elements in LST are same."
  (let ((first-elem (car lst))
        (result t))
    (while lst
      (unless (string= first-elem (car lst))
        (setq result nil))
      (setq lst (cdr lst)))
    result))

(defun eli/quit-minibuffer (&rest _)
  (if (fboundp 'minibuffer-quit-recursive-edit)
      (minibuffer-quit-recursive-edit)
    (abort-recursive-edit)))

(defvar eli/move-file-list '())

(defun eli/move-file (files)
  "Move FILES to the specific directory."
  (if-let*
      ((same-parent-p (directory-same-p
                       (mapcar #'file-name-parent-directory files)))
       (parent (eli/common-parent files))
       (dest (read-directory-name
              (format "Move %d file(s) to: " (length files))
              parent)))
      (progn
        (unless (file-exists-p dest)
          (make-directory dest))
        (when (y-or-n-p (format "Move %d file(s) to %s ?"
                                (length files)
                                dest))
          (dolist (file files)
            (if-let ((bf (get-file-buffer file))
                     (new-name (file-name-concat dest
                                                 (file-name-nondirectory file))))
                (with-current-buffer bf
                  (rename-file file new-name)
                  (set-visited-file-name new-name)
                  (set-buffer-modified-p nil)
                  (push (cons bf (expand-file-name dest))
                        eli/move-file-list)
                  ;; (setq default-directory (expand-file-name dest))
                  (recentf-push new-name))
              (rename-file file dest)))))
    (error "All files must be in the same directory!")))

(defun eli/move-file-after-action (&rest _)
  (dolist (pair eli/move-file-list)
    (with-current-buffer (car pair)
      (setq default-directory (cdr pair))))
  (setq eli/move-file-list nil)
  (eli/quit-minibuffer))

(defun eli/delete-file (files)
  (when (y-or-n-p (format "Move %d file(s) to the trash?" (length files)))
    (dolist (file files)
      (dired-delete-file file 'always t))))

(defun eli/copy-file (files)
  (let*
      ((parent (eli/common-parent files))
       (dest (read-directory-name
              (format "Move %d file(s) to: " (length files))
              parent)))
    (unless (file-exists-p dest)
      (make-directory dest))
    (when (y-or-n-p (format "Copy %d file(s) to %s ?"
                            (length files)
                            dest))
      (dolist (file files)
        (copy-file file dest)))))

(defvar eli/multitarget-actions '(eli/copy-file
                                  eli/move-file
                                  eli/delete-file))

;;;; provide
(provide 'lib-embark)
;;; lib-embark.el ends here.
