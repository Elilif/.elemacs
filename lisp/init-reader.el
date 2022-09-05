;; init-reader.el --- Initialize reader configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 by Eli

;; Author: Eli <eli.q.qian@gmail.com>
;; URL: https://github.com/Elilif/.emacs.d

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

(elemacs-require-package 'pdf-tools)
(pdf-loader-install)
(defun +pdf-keyboard-select-region (&optional all-pages-p)
  "Ref: https://github.com/dalanicolai/dala-emacs-lisp/blob/9662aa2ab993157e6e7587385d27c48ed50a1a50/pdf-keyboard-highlight.el#L79"
  (interactive "P")
  (pdf-view-deactivate-region)
  (let* ((pages (if all-pages-p nil (pdf-view-current-page)))
	 (candidates (mapcar (lambda (x)
			       (list (cdar (cdr x))
				     (cdar x)
                                     (cdar (cddr x))))
			     (pdf-info-search-regexp (read-string "Regexp: ") pages)))
         (page-edges-list (alist-get (completing-read "Select correct context: " candidates)
                                     candidates nil nil 'equal))
	 (edges-list (cadr page-edges-list))
         (edges (append (cl-subseq (car edges-list) 0 2) (cl-subseq (car (last edges-list)) 2 4)))
	 )
    (pdf-view-goto-page (car page-edges-list))
    (setq pdf-view-active-region (list edges))
    (pdf-view--push-mark)
    (pdf-view-display-region)))

(with-eval-after-load 'pdf-tools
  (define-key pdf-view-mode-map (kbd "v") #'+pdf-keyboard-select-region))

(elemacs-require-package 'saveplace-pdf-view)
(with-eval-after-load 'pdf-tools
  (require 'saveplace-pdf-view))

(elemacs-require-package 'org-noter)
(with-eval-after-load 'org-noter
  (setq org-noter-auto-save-last-location t)
  (setq org-noter-doc-split-fraction '(0.52 0.48))
  (setq org-noter-notes-search-path '("~/Dropbox/org/roam"))
  (setq org-noter-always-create-frame t)

  (defun org-noter--focus-notes-region (view-info)
    (org-noter--with-selected-notes-window
     (if (org-noter--session-hide-other session)
         (save-excursion
           (goto-char (org-element-property :begin (org-noter--parse-root)))
           (outline-hide-subtree))
       (org-cycle-hide-drawers 'all))

     (let* ((notes-cons (org-noter--view-info-notes view-info))
            (regions (or (org-noter--view-info-regions view-info)
                         (org-noter--view-info-prev-regions view-info)))
            (point-before (point))
            target-region
            point-inside-target-region)
       (cond
        (notes-cons
         (dolist (note-cons notes-cons) (org-noter--show-note-entry session (car note-cons)))
         (save-excursion
           (goto-char (org-entry-beginning-position))
           (let* ((hl (org-element-context))
                  (hl-begin (plist-get  (plist-get hl 'headline) :begin))
                  (hl-end (1- (plist-get  (plist-get hl 'headline) :contents-begin))))
             (remove-overlays hl-begin hl-end 'org-noter-current-hl t)))
         (setq target-region (or (catch 'result (dolist (region regions)
                                                  (when (and (>= point-before (car region))
                                                             (or (save-restriction (goto-char (cdr region)) (eobp))
                                                                 (< point-before (cdr region))))
                                                    (setq point-inside-target-region t)
                                                    (throw 'result region))))
                                 (car regions)))

         (let ((begin (car target-region)) (end (cdr target-region)) num-lines
               (target-char (if point-inside-target-region
                                point-before
                              (org-noter--get-properties-end (caar notes-cons))))
               (window-start (window-start)) (window-end (window-end nil t)))
           (setq num-lines (count-screen-lines begin end))

           (cond
            ((> num-lines (window-height))
             (goto-char begin)
             (recenter 0))

            ((< begin window-start)
             (goto-char begin)
             (recenter 0))

            ((> end window-end)
             (goto-char end)
             (recenter -2)))

           (goto-char target-char)))

        (t (org-noter--show-note-entry session (org-noter--parse-root)))))

     (org-cycle-show-empty-lines t)
     (org-cycle-hide-drawers 'all)
     (save-excursion
       (goto-char (org-entry-beginning-position))
       (let* ((hl (org-element-context))
              (hl-begin (plist-get  (plist-get hl 'headline) :begin))
              (hl-end (1- (plist-get  (plist-get hl 'headline) :contents-begin)))
              (hl-ov (make-overlay hl-begin hl-end)))
         (overlay-put hl-ov 'face 'mindre-keyword)
         (overlay-put hl-ov 'org-noter-current-hl t)
         )))))

(elemacs-require-package 'org-pdftools)
(with-eval-after-load 'org-noter
  (org-pdftools-setup-link))

(elemacs-require-package 'toc-mode)

(provide 'init-reader)
;;; init-reader.el ends here.
