;; lib-embark.el --- Initialize lib-embark configurations.	-*- lexical-binding: t; -*-

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
  (let ((default-directory "~/Documents/org-images/"))
    (call-interactively 'find-file)))

(defun eli/remove-image-preview-hook ()
  (remove-hook 'post-command-hook #'eli/image-preview)
  (posframe--kill-buffer "*image*")
  (remove-hook 'minibuffer-exit-hook #'eli/remove-image-preview-hook))

(advice-add 'eli/select-images
            :before
            (lambda (&rest _args)
              (add-hook 'post-command-hook #'eli/image-preview)
              (add-hook 'minibuffer-exit-hook #'eli/remove-image-preview-hook)))


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

(defvar-keymap embark-multi-category-map
  "k" #'kill-buffer
  "i" #'embark-insert
  "w" #'embark-copy-as-kill
  "r" #'tabspaces-remove-selected-buffer)

(defvar eli/vertico-marked-list nil
  "List of marked candidates in minibuffer.")

(defun eli/vertico-mark ()
  "Mark candidates in minibuffer"
  (interactive)
  (let*
	  ((selected (embark--vertico-selected))
	   (target (cdr selected)))
	(if (member target eli/vertico-marked-list)
		(setq eli/vertico-marked-list (delete target eli/vertico-marked-list))
	  (push target eli/vertico-marked-list))))

(defun eli/vertico-marked-p (candidate)
  "Return t if CANDIDATE is in `eli/vertico-marked-list'."
  (member (concat vertico--base candidate) eli/vertico-marked-list))

(cl-defgeneric eli/vertico--format-candidate (cand prefix suffix index _start)
  "Format CAND given PREFIX, SUFFIX and INDEX."
  (if (eli/vertico-marked-p cand)
	  (add-face-text-property 0 (length cand) 'embark-collect-marked nil cand)
	(remove-text-properties 0 (length cand) '(face) cand))
  (setq cand (vertico--display-string (concat prefix cand suffix "\n")))
  (when (= index vertico--index)
    (add-face-text-property 0 (length cand) 'vertico-current 'append cand))
  cand)

(advice-add #'vertico--format-candidate :override #'eli/vertico--format-candidate)

(defun eli/vertico-marked-list-clean ()
  "Initialize `eli/vertico-marked-list' and `eli/vertico-mark-type'."
  (setq eli/vertico-marked-list nil))

(add-hook 'minibuffer-setup-hook #'eli/vertico-marked-list-clean)

(defun eli/embark--maybe-transform-candidates ()
  "Collect candidates and see if they all transform to the same type.
Return a plist with keys `:type', `:orig-type', `:candidates', and
`:orig-candidates'."
  (pcase-let ((`(,type . ,candidates)
               (if eli/vertico-marked-list
				   (cons (car (embark--vertico-selected))
						 eli/vertico-marked-list)
				 (run-hook-with-args-until-success 'embark-candidate-collectors))))
    (when (eq type 'file)
      (let ((dir (embark--default-directory)))
        (setq candidates
              (mapcar (lambda (cand)
                        (abbreviate-file-name (expand-file-name cand dir)))
                      candidates))))
    (append
     (list :orig-type type :orig-candidates candidates)
     (or (when candidates
           (when-let ((transformer (alist-get type embark-transformer-alist)))
             (pcase-let* ((`(,new-type . ,first-cand)
                           (funcall transformer type (car candidates))))
               (let ((new-candidates (list first-cand)))
                 (when (cl-every
                        (lambda (cand)
                          (pcase-let ((`(,t-type . ,t-cand)
                                       (funcall transformer type cand)))
                            (when (eq t-type new-type)
                              (push t-cand new-candidates)
                              t)))
                        (cdr candidates))
                   (list :type new-type
                         :candidates (nreverse new-candidates)))))))
         (list :type type :candidates candidates)))))

(advice-add #'embark--maybe-transform-candidates :override #'eli/embark--maybe-transform-candidates)

;;;; provide
(provide 'lib-embark)
;;; lib-embark.el ends here.
