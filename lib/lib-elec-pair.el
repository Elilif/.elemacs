;; lib-elec-pair.el --- Initialize lib-elec-pair configurations.    -*- lexical-binding: t; -*-

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

(defun electric-pair-post-self-insert-function ()
  "Member of `post-self-insert-hook'.  Do main work for `electric-pair-mode'.
If the newly inserted character C has delimiter syntax, this
function may decide to insert additional paired delimiters, or
skip the insertion of the new character altogether by jumping
over an existing identical character, or do nothing.

The decision is taken by order of preference:

* According to `use-region-p'.  If this returns non-nil this
  function will unconditionally \"wrap\" the region in the
  corresponding delimiter for C;

* According to C alone, by looking C up in the tables
  `electric-pair-pairs' or `electric-pair-text-pairs' (which
  see);

* According to C's syntax and the syntactic state of the buffer
  (both as defined by the major mode's syntax table).  This is
  done by looking up the variables
 `electric-pair-inhibit-predicate', `electric-pair-skip-self'
  and `electric-pair-skip-whitespace' (which see)."
  (let* ((pos (and electric-pair-mode (electric--after-char-pos)))
         (skip-whitespace-info))
    (pcase (electric-pair-syntax-info last-command-event)
      (`(,syntax ,pair ,unconditional ,_)
       (cond
        ((null pos) nil)
        ;; Wrap a pair around the active region.
        ;;
        ((and (memq syntax '(?\( ?\) ?\" ?\$)) (use-region-p))
         ;; FIXME: To do this right, we'd need a post-self-insert-function
         ;; so we could add-function around it and insert the closer after
         ;; all the rest of the hook has run.
         (if (or (eq syntax ?\")
                 (and (eq syntax ?\))
                      (>= (point) (mark)))
                 (and (not (eq syntax ?\)))
                      (>= (mark) (point))))
             (save-excursion
               (goto-char (mark))
               (electric-pair--insert pair))
           (delete-region pos (1- pos))
           (electric-pair--insert pair)
           (goto-char (mark))
           (electric-pair--insert last-command-event)))
        ;; Backslash-escaped: no pairing, no skipping.
        ((save-excursion
           (goto-char (1- pos))
           (not (zerop (% (skip-syntax-backward "\\") 2))))
         nil)
        ;; Skip self.
        ((and (memq syntax '(?\) ?\" ?\$))
              (and (or unconditional
                       (if (functionp electric-pair-skip-self)
                           (electric-pair--save-literal-point-excursion
                            (goto-char pos)
                            (funcall electric-pair-skip-self last-command-event))
                         electric-pair-skip-self))
                   (save-excursion
                     (when (and (not (and unconditional
                                          (eq syntax ?\")))
                                (setq skip-whitespace-info
                                      (if (and (not (eq electric-pair-skip-whitespace 'chomp))
                                               (functionp electric-pair-skip-whitespace))
                                          (funcall electric-pair-skip-whitespace)
                                        electric-pair-skip-whitespace)))
                       (funcall electric-pair-skip-whitespace-function))
                     (eq (char-after) last-command-event))))
         ;; This is too late: rather than insert&delete we'd want to only
         ;; skip (or insert in overwrite mode).  The difference is in what
         ;; goes in the undo-log and in the intermediate state which might
         ;; be visible to other post-self-insert-hook.  We'll just have to
         ;; live with it for now.
         (when skip-whitespace-info
           (funcall electric-pair-skip-whitespace-function))
         (delete-region (1- pos) (if (eq skip-whitespace-info 'chomp)
                                     (point)
                                   pos))
         (forward-char))
        ;; Insert matching pair.
        ((and (memq syntax '(?\( ?\" ?\$))
              (not overwrite-mode)
              ;;; always use `electric-pair-inhibit-predicate'
              (not (electric-pair--save-literal-point-excursion
                    (goto-char pos)
                    (funcall electric-pair-inhibit-predicate
                             last-command-event))))
         (save-excursion (electric-pair--insert pair))))))))

(defmacro eli/add-mode-pairs (hook pairs)
  `(add-hook ,hook
             (lambda ()
               (setq-local electric-pair-pairs (append electric-pair-pairs ,pairs))
               (setq-local electric-pair-text-pairs electric-pair-pairs))))

(eli/add-mode-pairs 'org-mode-hook '((?/ . ?/)
                                     (?= . ?=)
                                     (?* . ?*)
                                     (?+ . ?+)
                                     (?~ . ?~)
                                     (?_ . ?_)))

(defun eli/electric-pair-inhibit (char)
  (cond
   ((eq major-mode 'org-mode)
    (or
     (and
      (member char '(?/ ?= ?* ?+ ?~ ?_))
      (not (member (char-before (1- (point))) '(?\ ?\t))))
     (and
      (member char '(?/ ?= ?* ?+ ?~ ?_))
      (member (char-before (1- (point))) '(?\ ?\t))
      (letrec ((eli/delete-pair-advice (lambda (&rest _args)
                                         (add-hook 'post-self-insert-hook eli/delete-pair)
                                         (advice-remove 'electric-pair--insert eli/delete-pair-advice)))
               (eli/delete-pair (lambda ()
                                  (when (eq (char-before) ?\ )
                                    (delete-char 1))
                                  (remove-hook 'post-self-insert-hook eli/delete-pair))))
        (advice-add 'electric-pair--insert :after eli/delete-pair-advice))
      nil)
     (and
      (org-inside-LaTeX-fragment-p)
      (member char '(?/ ?= ?* ?+ ?~ ?_)))))
   (t
    (if electric-pair-preserve-balance
        (electric-pair-inhibit-if-helps-balance char)
      (electric-pair-conservative-inhibit char)))))


;;;; provide
(provide 'lib-elec-pair)
;;; lib-elec-pair.el ends here.
