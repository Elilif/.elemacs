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

(defvar eli/vertico-marked-list '())

(defun eli/vertico-mark ()
  (interactive)
  (let ((target (plist-get (car (embark--targets)) :target)))
	(if (member target eli/vertico-marked-list)
		(setq eli/vertico-marked-list (delete target eli/vertico-marked-list))
	  (add-to-list 'eli/vertico-marked-list target))
	(vertico--display-candidates (vertico--arrange-candidates))))

(cl-defgeneric eli/vertico--format-candidate (cand prefix suffix index _start)
  "Format CAND given PREFIX, SUFFIX and INDEX."
  (if (member (cdr (embark--refine-multi-category 'multi-category cand)) eli/vertico-marked-list)
	  (add-face-text-property 0 (length cand) 'embark-collect-marked nil cand)
	(remove-text-properties 0 (length cand) 'embark-collect-marked cand))
  (setq cand (vertico--display-string (concat prefix cand suffix "\n")))
  (when (= index vertico--index)
    (add-face-text-property 0 (length cand) 'vertico-current 'append cand))
  cand)
(advice-add #'vertico--format-candidate :override #'eli/vertico--format-candidate)

(defun eli/vertico-marked-list-clean ()
  (setq eli/vertico-marked-list '()))

(add-hook 'minibuffer-setup-hook #'eli/vertico-marked-list-clean)

(defun eli/embark-act (&optional arg)
  (interactive "P")
  (let* ((targets (or (embark--targets) (user-error "No target found")))
         (indicators (mapcar #'funcall embark-indicators))
         (default-done nil))
    (when arg
      (if (minibufferp)
          (embark-toggle-quit)
        (setq targets (embark--rotate targets (prefix-numeric-value arg)))))
    (unwind-protect
        (while
            (let* ((target (car targets))
                   (action
                    (or (embark--prompt
                         indicators
                         (let ((embark-default-action-overrides
                                (if default-done
                                    `((t . ,default-done))
                                  embark-default-action-overrides)))
                           (embark--action-keymap (plist-get target :type)
                                                  (cdr targets)))
                         targets)
                        (user-error "Canceled")))
                   (default-action (or default-done
                                       (embark--default-action
                                        (plist-get target :type)))))
              (cond
               ;; When acting twice in the minibuffer, do not restart
               ;; `embark-act'.  Otherwise the next `embark-act' will
               ;; find a target in the original buffer.
               ((eq action #'embark-act)
                (message "Press an action key"))
               ((eq action #'embark-cycle)
                (setq targets (embark--rotate
                               targets (prefix-numeric-value prefix-arg))))
               (t
                ;; if the action is non-repeatable, cleanup indicator now
                (let ((repeat (embark--action-repeatable-p action)))
                  (unless repeat (mapc #'funcall indicators))
                  (condition-case err
                      (if eli/vertico-marked-list
						  (if (memq action embark-multitarget-actions)
							  (embark--quit-and-run action eli/vertico-marked-list)
							(embark--quit-and-run #'mapc action eli/vertico-marked-list))
						(embark--act
						 action
						 (if (and (eq action default-action)
								  (eq action embark--command)
								  (not (memq action embark-multitarget-actions)))
							 (embark--orig-target target)
						   target)
						 (embark--quit-p action)))
                    (user-error
                     (funcall (if repeat #'message #'user-error)
                              "%s" (cadr err))))
                  (when-let (new-targets (and repeat (embark--targets)))
                    ;; Terminate repeated prompter on default action,
                    ;; when repeating. Jump to the region type if the
                    ;; region is active after the action, or else to the
                    ;; current type again.
                    (setq default-done #'embark-done
                          targets
                          (embark--rotate
                           new-targets
                           (or (cl-position-if
                                (let ((desired-type
                                       (if (eq repeat t)
                                           (plist-get (car targets) :type)
                                         repeat)))
                                  (lambda (x)
                                    (eq (plist-get x :type) desired-type)))
                                new-targets)
                               0)))))))))
      (mapc #'funcall indicators))))

(advice-add #'embark-act :override #'eli/embark-act)

;;;; provide
(provide 'lib-embark)
;;; lib-embark.el ends here.
