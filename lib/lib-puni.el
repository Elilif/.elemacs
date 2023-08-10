;; lib-puni.el --- Initialize lib-puni configurations.	-*- lexical-binding: t; -*-

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

;; the following code are taken from `expand-region'
;;;###autoload
(defun er/mark-word ()
  "Mark the entire word around or in front of point."
  (interactive)
  (let ((word-regexp "\\sw"))
    (when (or (looking-at word-regexp)
              (looking-back word-regexp (line-beginning-position)))
      (skip-syntax-forward "w")
      (set-mark (point))
      (skip-syntax-backward "w"))))

;;;###autoload
(defun er/mark-symbol ()
  "Mark the entire symbol around or in front of point."
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw"))
	(when (or (looking-at symbol-regexp)
			  (looking-back symbol-regexp (line-beginning-position)))
	  (skip-syntax-forward "_w")
	  (set-mark (point))
	  (skip-syntax-backward "_w"))))

;;;###autoload
(defun er/mark-sentence ()
  "Marks one sentence."
  (interactive)
  (forward-char 1)
  (backward-sentence 1)
  (set-mark (point))
  (forward-sentence 1)
  (exchange-point-and-mark))

(defun eli/mark-symbol-maybe ()
  (let ((current-length (- (region-end)
						   (region-beginning)))
		(symbol-length (length (thing-at-point 'symbol))))
	(if (/= current-length symbol-length)
		(er/mark-symbol)
	  (setq eli/expand-region-count
			(1+ eli/expand-region-count))
	  (funcall (nth eli/expand-region-count
					eli/expand-region-commands)))))

(defvar eli/expand-region-count 1)
(defvar eli/expand-region-commands '(er/mark-word
									 eli/mark-symbol-maybe
									 er/mark-sentence))

;;;###autoload
(defun eli/expand-region ()
  (interactive)
  (if (eq last-command this-command)
	  (progn
		(cond
		 ((derived-mode-p 'prog-mode)
		  (puni-expand-region))
		 (t (if (< eli/expand-region-count 3)
				(funcall (nth eli/expand-region-count
							  eli/expand-region-commands))
			  (puni-expand-region))))
		(setq eli/expand-region-count
			  (1+ eli/expand-region-count)))
	(if (thing-at-point 'word)
		(er/mark-word)
	  (puni-expand-region))
	(setq eli/expand-region-count 1)))

;; SRC: https://github.com/AmaiKinono/puni/issues/29
;;;###autoload
(defun eli/puni-hungry-backward-delete-char (&optional n)
  "Respect major mode keybindings.

See `puni-backward-delete-char' for more information."
  (interactive "p")
  (unless (bobp)
    (setq n (or n 1))
    (if (puni-region-balance-p (- (point) n) (point))
        (unwind-protect (progn (puni-mode -1)
                               (execute-kbd-macro (kbd "DEL") n))
          (puni-mode))
      (puni-backward-delete-char n))))


;;;; LaTeX
(setq eli/tex-delimiters (cl-loop for (l r)
								  in '(( "(" ")" )
									   ( "[" "]" )
									   ( "\\{" "\\}" )
									   ( "\\langle" "\\rangle" )
									   ( "\\lvert" "\\rvert" )
									   ( "\\lVert" "\\rVert" ))
								  nconc
								  (cl-loop for (pre-l pre-r)
										   in '(( "" "" )
												( "\\left"  "\\right")
												( "\\bigl"  "\\bigr")  ("\\big"  "\\big")
												( "\\biggl" "\\biggr") ("\\bigg" "\\bigg")
												( "\\Bigl"  "\\Bigr")  ("\\Big"  "\\Big")
												( "\\Biggl" "\\Biggr") ("\\Bigg" "\\Bigg"))
										   collect (cons (concat pre-l l) (concat pre-r r)))))

(setq eli/tex-delimiters (append eli/tex-delimiters '(("\\[" . "\\]")
													  ("\\(" . "\\)"))))

(defun eli/latex-backward-sexp-1 (&optional arg)
  (dotimes (_ (abs arg))
	(let ((case-fold-search nil))
	  (if-let* ((lim (save-excursion
					   (re-search-backward (concat "^\\(" paragraph-start "\\)") nil
										   'move)
					   (point)))
				(rcmd (when (looking-back "\\(?:\\s-\\|^\\)\\(\\\\.*?\\)\\s-?" lim)
						(match-string 1)))
				(lcmd (car-safe (rassoc rcmd eli/tex-delimiters))))
		  (re-search-backward (regexp-quote lcmd))
		(if (and rcmd (equal rcmd (car-safe (assoc rcmd eli/tex-delimiters))))
			(user-error "Beginning of sexp!")
		  (goto-char (or (scan-sexps (point) -1) (buffer-end arg)))
		  (if (< arg 0) (backward-prefix-chars)))))))

(defun eli/latex-forward-sexp-1 (&optional arg)
  (dotimes (_ (abs arg))
	(let ((case-fold-search nil))
	  (if-let* ((lcmd (when (looking-at "\\s-?\\(\\\\.*?\\)\\s-")
						(match-string 1)))
				(rcmd (cdr-safe (assoc lcmd eli/tex-delimiters #'equal))))
		  (re-search-forward (regexp-quote rcmd))
		(if (or (and lcmd (equal lcmd (cdr-safe (rassoc lcmd eli/tex-delimiters))))
				(and (eq (char-before) ?\\)
					 (memq (char-after) '(?\( ?\[ ?\{)))
				(and (org-inside-latex-macro-p)
					 (not (eq (char-before) ?\}))
					 (eq (char-after) ?\[))
				(and (org-inside-latex-macro-p)
					 (not (eq (char-before) ?\ ))
					 (eq (char-after) ?\()))
			(user-error "End of sexp!")
		  (goto-char (or (scan-sexps (point) 1) (buffer-end arg))))))))

(defun eli/latex-forward-sexp (&optional arg)
  (condition-case _
	  (if (> arg 0)
		  (eli/latex-forward-sexp-1 arg)
		(eli/latex-backward-sexp-1 arg))
	(scan-error (user-error (if (> arg 0)
								"No next sexp"
							  "No previous sexp")))))

(defun eli/puni-latex-mark ()
  (puni-beginning-of-sexp)
  (skip-chars-forward "[:space:]")
  (set-mark (point))
  (puni-end-of-sexp)
  (skip-chars-backward "[:space:]"))

;;;###autoload
(defun eli/puni-latex-raise ()
  (interactive)
  (save-excursion
	(eli/puni-latex-mark)
	(puni-raise)
	(deactivate-mark)))

(defun eli/puni-latex--wrap-region (beg end ldelim rdelim)
  (save-excursion
	(goto-char end)
	(insert (concat " " rdelim))
	(goto-char beg)
	(insert (concat ldelim " "))))

;;;###autoload
(defun eli/puni-latex-rewrap (lpair &optional new)
  (interactive (list (completing-read "Select: " eli/tex-delimiters)
					 current-prefix-arg))
  (let ((rpair (cdr (assoc lpair eli/tex-delimiters #'equal)))
		beg end)
	(save-excursion
	  (unless (region-active-p)
		(eli/puni-latex-mark))
	  (unless new
		(puni-raise))
	  (setq beg (region-beginning)
			end (region-end))
	  (deactivate-mark)
	  (eli/puni-latex--wrap-region beg end lpair rpair))))

(defun eli/puni--forward-sexp-wrapper (orig &rest arg)
  (if (eq major-mode 'org-mode)
	  (let ((forward-sexp-function #'eli/latex-forward-sexp))
		(apply orig arg))
	(apply orig arg)))
(advice-add 'puni--forward-sexp-wrapper :around #'eli/puni--forward-sexp-wrapper)
(advice-add 'kill-sexp :around #'eli/puni--forward-sexp-wrapper)

;;;; provide
(provide 'lib-puni)
;;; lib-puni.el ends here.
