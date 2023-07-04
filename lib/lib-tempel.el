;; lib-tempel.el --- Initialize lib-tempel configurations.	-*- lexical-binding: t; -*-

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

(defun tempel-include (elt)
  (when (eq (car-safe elt) 'i)
    (if-let (template (alist-get (cadr elt) (tempel--templates)))
        (cons 'l template)
	  (message "Template %s not found" (cadr elt))
      nil)))

(add-to-list 'tempel-user-elements #'tempel-include)

(defun eli/abbrev--case-fixed (args)
  (when (length= args 3)
	(setq args (append args '(nil))))
  (plist-put args :case-fixed t)
  args)
(advice-add 'define-abbrev :filter-args #'eli/abbrev--case-fixed)

(defun eli/tempel-expand ()
  "Complete with CAPF."
  (let ((completion-at-point-functions (list #'tempel-expand))
		completion-cycle-threshold)
	(tempel--save)
	(completion-at-point)))

(defun tempel-setup-capf ()
  ;; Add the Tempel Capf to `completion-at-point-functions'.
  ;; `tempel-expand' only triggers on exact matches. Alternatively use
  ;; `tempel-complete' if you want to see all matches, but then you
  ;; should also configure `tempel-trigger-prefix', such that Tempel
  ;; does not trigger too often when you don't expect it. NOTE: We add
  ;; `tempel-expand' *before* the main programming mode Capf, such
  ;; that it will be tried first.
  (setq-local completion-at-point-functions
			  (cons #'eli/tempel-expand
					completion-at-point-functions)))

(defun eli/tempel--exit (templates region name status)
  "Exit function for completion for template NAME and STATUS.
TEMPLATES is the list of templates.
REGION are the current region bounds."
  (unless (eq status 'exact)
    (when-let ((sym (intern-soft name))
               (template (alist-get sym templates))
			   (plist template))
	  (while (and plist (not (keywordp (car plist))))
		(pop plist))
      (when (or (eval (plist-get plist :auto) 'lexical)
				(eq this-command 'smarter-tab-to-expand))
		(tempel--delete-word name)
		(when tempel-trigger-prefix
          (tempel--delete-word tempel-trigger-prefix))
		(tempel--insert template region)))))

(defun eli/bounds-of-thing-at-point (thing)
  (let* ((bounds (bounds-of-thing-at-point thing))
		 (a (car bounds))
		 (b (cdr bounds))
		 a-r b-r)
	(when bounds
	  (save-excursion
		(goto-char a)
		(setq b-r (or (1- (re-search-forward " \\|$" (line-end-position) t)) 0)))
	  (save-excursion
		(goto-char b)
		(setq a-r (or (1+ (re-search-backward "^\\| " (line-beginning-position) t)) 0)))
	  (cons (min a-r a)
			(max b-r b)))))

(defun eli/tempel--get-prefix-bounds ()
  (let* ((beg (save-excursion
				(re-search-backward " "
									(line-beginning-position) 'noerror)))
		 (beg (if beg (1+ beg) (line-beginning-position))))
	(cons beg (point))))

(defun tempel--prefix-bounds ()
  "Return prefix bounds."
  (if tempel-trigger-prefix
      (let ((end (point))
            (beg (save-excursion
                   (search-backward tempel-trigger-prefix
                                    (line-beginning-position) 'noerror))))
        (when (and beg (save-excursion
                         (not (re-search-backward "\\s-" beg 'noerror))))
          (cons (+ beg (length tempel-trigger-prefix)) end)))
    ;; (eli/bounds-of-thing-at-point 'symbol)
	(eli/tempel--get-prefix-bounds)))


(defvar smarter-tab-to-expand-in-use nil)

(defun smarter-tab-to-expand ()
  "Try to `org-cycle', `tempel-expand' at current cursor position.
If all failed, try to complete the common part with `indent-for-tab-command'."
  (interactive)
  (if (and (not smarter-tab-to-expand-in-use)
		   (featurep 'tempel))
      (let ((old-point (point))
			(old-tick (buffer-chars-modified-tick))
			(func-list
			 (if (equal major-mode 'org-mode) '(org-cycle tempel-expand tempel-next)
               '(tempel-expand tempel-next))))
		(catch 'func-suceed
		  (setq smarter-tab-to-expand-in-use t)
          (dolist (func func-list)
			(ignore-errors (call-interactively func))
			(unless (and (eq old-point (point))
						 (eq old-tick (buffer-chars-modified-tick)))
			  (setq smarter-tab-to-expand-in-use nil)
              (throw 'func-suceed t)))
		  (setq smarter-tab-to-expand-in-use nil)
          (indent-for-tab-command)))
	(indent-for-tab-command)))

(defun eli/org-inside-LaTeX-fragment-p ()
  (and (fboundp #'org-inside-LaTeX-fragment-p)
	   (org-inside-LaTeX-fragment-p)))

(defun eli/tempel-get-tex-object ()
  "Get tex object before point."
  (save-excursion
	(pcase (progn
			 (skip-chars-backward " \t" (line-beginning-position))
			 (char-before))
	  ((or ?\) ?\])
	   (backward-sexp)
	   (point))
	  (?\}
	   (cl-loop do (backward-sexp)
				while (= (char-before) ?\}))
	   (or (save-excursion
			 (re-search-backward "\\\\" (line-beginning-position) 'noerror))
		   (point)))
	  ((pred (lambda (char)
			   (not (memq char '(?\C-j)))))
	   (backward-word)
	   (point)))))

(defun eli/tempel-tex-smart-kill ()
  (when-let* ((beg (eli/tempel-get-tex-object))
			  (end (point)))
	(kill-region beg end)))

(defun eli/tempel-tex-smart-paste ()
  "Paste text killed by `eli/latex-smart-kill'."
  (let ((temp (string-clean-whitespace (current-kill 0))))
	(if (string-match "^(\\(.*\\))$" temp)
		(match-string 1 temp)
	  temp)))

;; C/C++ mode
;; (defun eli/c-fun-has-namespace-p (namespace)
;;   "Predicate whether the current function has NAMESPACE namespace."
;;   (save-excursion
;;     (c-beginning-of-defun)
;;     (unless (re-search-forward
;;              (concat "using namespace "
;;                      namespace
;;                      ";")
;;              (save-excursion
;;                (c-end-of-defun)
;;                (point)) 'no-errer)
;;       (concat namespace "::"))))


;;;; provide
(provide 'lib-tempel)
;;; lib-tempel.el ends here.
