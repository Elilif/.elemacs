;; lib-helpful.el --- Initialize lib-helpful configurations.	-*- lexical-binding: t; -*-

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

(defun +helpful-switch-to-buffer (buffer-or-name)
  "Switch to helpful BUFFER-OR-NAME."
  (if-let ((helpful-window (cl-some (lambda (window)
									  (with-current-buffer (window-buffer window)
										(when (eq major-mode 'helpful-mode)
										  window)))
									(window-list))))
	  (progn
		(select-window helpful-window)
		(switch-to-buffer buffer-or-name))
	(pop-to-buffer buffer-or-name)))

(defun helpful-set-arguments-face (&rest _args)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (limit (save-excursion
                   (re-search-forward "^Source Code\n" nil t))))
      (while (re-search-forward
              "\\<[-A-Z]+\\>" limit t)
        (overlay-put (make-overlay
                      (match-beginning 0) (match-end 0))
                     'face 'help-argument-name)))))

;;; SRC:
;;; https://gist.github.com/twlz0ne/f93debe098e0e39ebce5476b62c8ebbb#file-advice-remove-button-el
(defun function-advices (function)
  "Return FUNCTION's advices.
Last-Updated 2022-06-29 00:01:07 +8000"
  (let ((flist (indirect-function function)) advices)
    (when (and (consp flist)
               (or (eq 'macro (car flist))
                   (and (autoloadp flist) (memq (nth 4 flist) '(macro t)))))
      (setq flist (cdr flist)))
    (while (advice--p flist)
      (setq advices `(,@advices ,(advice--car flist)))
      (setq flist (advice--cdr flist)))
    advices))

(defun helpful-remove-advice ()
  "Add a button to remove advice.
Based on @xuchunyang's work in https://emacs-china.org/t/advice/7566
Last-Updated 2022-06-29 00:01:07 +8000"
  (save-excursion
    (goto-char (point-min))
    (let* ((function helpful--sym)
		   (ad-list (function-advices function)))
      (while (re-search-forward "^\\(?:This \\(?:function\\|macro\\) has \\)?:[-a-z]+ advice: \\(.+\\)\\.?$" nil t)
        (let* ((name (string-trim (match-string 1) "[‘'`]" "[’']"))
               (symbol (intern-soft name))
               (advice (or symbol (car ad-list))))
          (when advice
            (when symbol
              (cl-assert (eq symbol (car ad-list))))
            (let ((inhibit-read-only t))
              (insert " » ")
              (insert-text-button
               "Remove"
               'cursor-sensor-functions `((lambda (&rest _) (message "%s" ',advice)))
               'help-echo (format "%s" advice)
               'action
               ;; In case lexical-binding is off
               `(lambda (_)
                  (when (yes-or-no-p (format "Remove %s ? " ',advice))
                    (message "Removing %s of advice from %s" ',function ',advice)
                    (advice-remove ',function ',advice)
                    (helpful-update)))
               'follow-link t))))
        (setq ad-list (cdr ad-list))))))

(defun eli/helpful-remove-advice-i (function)
  "Select and remove an advice for FUNCTION."
  (interactive (list (helpful--read-symbol
					  "Callable: "
					  (helpful--callable-at-point)
					  #'fboundp)))
  (let* ((ads (function-advices function))
		 (ads-alist (mapcar (lambda (sexp) (cons (prin1-to-string sexp)
												 (list sexp)))
							ads))
		 (ad (elemacs-completing-read "Select an advice: "
									  ads-alist)))
	(advice-remove function ad)))


;;;; provide
(provide 'lib-helpful)
;;; lib-helpful.el ends here.
