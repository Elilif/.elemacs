;; lib-consult.el --- Initialize consult	-*- lexical-binding: t; -*-

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

(cl-eval-when (compile)
  (require 'consult)
  (require 'consult-imenu))

;;;; consult lib
;; src: https://github.com/minad/consult/wiki
(defun consult--orderless-regexp-compiler (input type &rest _config)
  (setq input (orderless-pattern-compiler input))
  (cons
   (mapcar (lambda (r) (consult--convert-regexp r type)) input)
   (lambda (str) (orderless--highlight input str))))

;; OPTION 1: Activate globally for all consult-grep/ripgrep/find/...
;; (setq consult--regexp-compiler #'consult--orderless-regexp-compiler)

;; OPTION 2: Activate only for some commands, e.g., consult-ripgrep!
(defun consult--with-orderless (&rest args)
  (minibuffer-with-setup-hook
      (lambda ()
        (setq-local consult--regexp-compiler #'consult--orderless-regexp-compiler))
    (apply args)))

;; Make consult-imenu ignore group titles when searching with orderless
(defun my/consult-imenu-around-advice (ci-orig &rest r)
  "Patch orderless to inhibit matching group categories in consult-imenu."
  (if-let* ((config (cdr (seq-find (lambda (x) (derived-mode-p (car x)))
                                   consult-imenu-config)))
            (types (plist-get config :types))
            (types-regex (rx-to-string
                          `(and line-start (or ,@(mapcar #'cadr types)) ? ))))
      (cl-letf* ((of-orig (symbol-function 'orderless-filter))
                 ((symbol-function 'orderless-filter) ;patch pattern compiler within filter
                  (lambda (&rest r)
                    (cl-letf* ((opc-orig (symbol-function 'orderless-pattern-compiler))
                               ((symbol-function 'orderless-pattern-compiler)
                                (lambda (&rest r)
                                  (if (and (eq (length r) 1) ;single match string starts
                                           (string-match-p types-regex (car r)))
                                      (apply opc-orig r)
                                    (mapcar (lambda (x) ;replace beginning-of-string
                                              (if (string-match (regexp-quote "\\`" ) x)
                                                  (concat types-regex
                                                          (replace-match "\\b" nil t x))
                                                (concat types-regex ".*?" x)))
                                            (apply opc-orig r))))))
                      (apply of-orig r))))
                 (oh-orig (symbol-function 'orderless--highlight))
                 ((symbol-function 'orderless--highlight) ; patch highlighter to skip type
                  (lambda (regexps string)
                    (if-let* ((pref
                               (next-single-property-change 0 'consult--type string)))
                        (cl-letf* ((sm-orig (symbol-function 'string-match))
                                   ((symbol-function 'string-match)
                                    (lambda (re str)
                                      (funcall sm-orig re str (1+ pref)))))
                          (funcall oh-orig regexps string))
                      (funcall oh-orig regexps string)))))
        (apply ci-orig r))
    (apply ci-orig r)))

;;Pre-select nearest heading for consult-org-heading and consult-outline using
;;vertico
(defvar consult--previous-point nil
  "Location of point before entering minibuffer.
Used to preselect nearest headings and imenu items.")

(defun consult--set-previous-point ()
  "Save location of point. Used before entering the minibuffer."
  (setq consult--previous-point (point)))

(defun consult-vertico--update-choose (&rest _)
  "Pick the nearest candidate rather than the first after updating candidates."
  (when (and consult--previous-point
             (memq current-minibuffer-command
                   '(consult-org-heading consult-outline)))
    (setq vertico--index
          (max 0 ; if none above, choose the first below
               (1- (or (seq-position
                        vertico--candidates
                        consult--previous-point
                        (lambda (cand point-pos) ; counts on candidate list being sorted
                          (> (cl-case current-minibuffer-command
                               (consult-outline
                                (car (consult--get-location cand)))
                               (consult-org-heading
                                (get-text-property 0 'consult--candidate cand)))
                             point-pos)))
                       (length vertico--candidates))))))
  (setq consult--previous-point nil))

;; my customizations
;;;###autoload
(defun eli/consult-buffer()
  (interactive)
  (consult-buffer)
  (when (member (prefix-numeric-value current-prefix-arg) '(4 16 64))
    (delete-other-windows)))

;;;###autoload
(defun eli/consult-git-grep ()
  "Search with git grep for files in current git dir. "
  (interactive)
  (consult-git-grep (vc-root-dir)))

;;;###autoload
(defun eli/consult-org-file (&optional match)
  "Jump to an Org heading.

MATCH is as in org-map-entries and determine which
entries are offered."
  (interactive)
  (consult-org-heading match '(list org-agenda-file-inbox org-agenda-file-habit org-agenda-file-projects)))

;;;###autoload
(defun eli/consult-org-roam-heading (&optional match)
  "Jump to an Org-roam heading.

MATCH is as in org-map-entries and determine which
entries are offered."
  (interactive)
  (consult-org-heading match '(directory-files-recursively org-roam-directory "\\.org")))

;;;###autoload
(defun eli/consult-ripgrep-single-file (file-path)
  "Search single file use `consult-ripgrep'."
  (interactive)
  (let ((consult-project-function (lambda (_x) nil))
        (consult-ripgrep-args
         (concat "rg "
                 "--null "
                 "--line-buffered "
                 "--color=never "
                 "--line-number "
                 "--smart-case "
                 "--no-heading "
                 "--max-columns=1000 "
                 "--max-columns-preview "
                 "--with-filename "
                 (shell-quote-argument file-path))))
    (consult-ripgrep)))

;;;###autoload
(defun eli/consult-git-ripgrep (dir)
  "Search single file use `consult-ripgrep'."
  (interactive
   (list (read-directory-name "Select directory: " "~/.emacs.d/site-lisp/")))
  (let ((consult-project-function (lambda (_x) nil))
        (consult-ripgrep-args (string-replace "." "-g *.el ."
                                              consult-ripgrep-args)))
    (consult-ripgrep dir)))

;;;###autoload
(defun eli/info-search ()
  "Search info through `consult-info'."
  (interactive)
  (consult-info "elisp" "emacs"))

;;;; provide
(provide 'lib-consult)
;;; lib-consult.el ends here.
