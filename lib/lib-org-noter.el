;; lib-org-noter.el --- Initialize lib-org-noter configurations.	-*- lexical-binding: t; -*-

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

;;;###autoload
(defun eli/org-noter-screenshot ()
  "Capture screenshot and insert the image in the note."
  (interactive)
  (let ((org-download-image-org-width 500))
	(org-noter-insert-note)
	(goto-char (org-entry-end-position))
	(org-download-screenshot))
  (save-excursion
	(goto-char (org-entry-beginning-position))
	(org-fold-show-entry)
	(org-cycle-hide-drawers 'fold))
  (redisplay))

(defun eli/org-noter-set-highlight (&rest _arg)
  "Highlight current org-noter note."
  (with-current-buffer (org-noter--session-notes-buffer org-noter--session)
    (remove-overlays (point-min) (point-max) 'org-noter-current-hl t)
    (goto-char (org-entry-beginning-position))
    (let* ((hl (org-element-context))
		   (hl-begin (1+ (plist-get  (plist-get hl 'headline) :begin)))
		   (hl-end (1- (plist-get  (plist-get hl 'headline) :contents-begin)))
		   (hl-ov (make-overlay hl-begin hl-end)))
	  (overlay-put hl-ov 'face 'mindre-keyword)
	  (overlay-put hl-ov 'org-noter-current-hl t))
    (org-cycle-hide-drawers 'all)))

(defun eli/org-noter-back-to-current-window (orig-fun)
  (save-selected-window
    (call-interactively orig-fun)))

(defvar org-noter-move-functions
  '(org-noter-sync-prev-note
    org-noter-sync-next-note))

(defmacro eli/advise-org-noter-functions (functions)
  `(progn
     ,@(mapcar (lambda (command)
                 `(advice-add ',command :around
                              #'eli/org-noter-back-to-current-window))
               (eval functions))))

(eli/advise-org-noter-functions org-noter-move-functions)

;;;###autoload
(defun eli/org-noter-scroll-up-other-window (lines)
  (interactive "P")
  (with-selected-window (other-window-for-scrolling)
    (funcall (or (command-remapping #'pdf-view-scroll-up-or-next-page)
                 #'pdf-view-scroll-up-or-next-page)
             lines)))

;;;###autoload
(defun eli/org-noter-scroll-down-other-window (lines)
  (interactive "P")
  (with-selected-window (other-window-for-scrolling)
    (funcall (or (command-remapping #'pdf-view-scroll-down-or-previous-page)
                 #'pdf-view-scroll-down-or-previous-page)
             lines)))

(defun eli/org-noter-kill-outline (&rest _arg)
  (when (buffer-live-p (get-buffer "*Outline Notes*"))
	(kill-buffer "*Outline Notes*")))

;; custom for org-anki
(defun eli/org-noter--insert-heading (level title &optional newlines-number location)
  "Insert a new heading at LEVEL with TITLE.
The point will be at the start of the contents, after any
properties, by a margin of NEWLINES-NUMBER."
  (setq newlines-number (or newlines-number 1))
  (org-insert-heading nil t)
  (let* ((initial-level (org-element-property :level (org-element-at-point)))
         (changer (if (> level initial-level) 'org-do-demote 'org-do-promote))
         (number-of-times (abs (- level initial-level))))
    (dotimes (_ number-of-times) (funcall changer))
    (insert (org-trim (replace-regexp-in-string "\n" " " title)))

    (org-end-of-subtree)
    (unless (bolp) (insert "\n"))
    (org-N-empty-lines-before-current (1- newlines-number))

    (when location
      (org-entry-put nil org-noter-property-note-location (org-noter--pretty-print-location location))
      ;; custom for org-anki
      (when (member (prefix-numeric-value current-prefix-arg) '(4))
        (org-entry-put nil "NOANKI" "t"))

      (when org-noter-doc-property-in-notes
        (org-noter--with-valid-session
         (org-entry-put nil org-noter-property-doc-file (org-noter--session-property-text session))
         (org-entry-put nil org-noter--property-auto-save-last-location "nil"))))

    (run-hooks 'org-noter-insert-heading-hook)))


;;;; provide
(provide 'lib-org-noter)
;;; lib-org-noter.el ends here.
