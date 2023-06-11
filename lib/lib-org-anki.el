;; lib-org-anki.el --- Initialize lib-org-anki configurations.	-*- lexical-binding: t; -*-

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

(defun eli/org-headline-empty-p ()
  "Return non-nil if current headline is empty."
  (not (org-element-property :robust-begin (org-element-headline-parser))))

;;;###autoload
(defun eli/org-show-empty-headline ()
  "Display all empty headlines in current buffer."
  (interactive)
  (org-cycle-set-startup-visibility)
  (let ((count 0))
    (org-map-entries
     (lambda ()
       (when (eli/org-headline-empty-p)
         (org-fold-show-context 'empty-headline)
         (setq count (1+ count)))))
    (message "There are %d questions remain to be solved." count)))

(defun org-anki-skip ()
  "Skip headlines with \"noanki\" property or with `org-anki-prop-note-id'. 
Used by `org-anki-skip-function'"
  (if (or (string= "t" (org-entry-get nil "NOANKI"))
          (org-entry-get nil org-anki-prop-note-id)
          (eli/org-headline-empty-p))
      (point)))

;; speed up org-anki
(defun eli/org-anki-around (orig &rest args)
  (let ((org-mode-hook nil))
    (apply orig args)))


(defun eli/org-anki-sync-item (item)
  (org-anki-connect-request
   (org-anki--create-note-single item)
   (lambda (the-result)
     (message
      "org-anki: note succesfully updated: %s"
      the-result))
   (lambda (the-error)
     (org-anki--report-error
      "Couldn't update note, received: %s"
      the-error))))

(defmacro eli/org-anki-install (fun-name reg front &optional back)
  `(defun ,(intern (format "org-anki-sync-%s" fun-name)) ()
     (interactive)
     (let ((deck (org-anki--find-prop
                  org-anki-prop-deck org-anki-default-deck))
           (type (org-anki--find-prop
                  org-anki-note-type org-anki-default-note-type)))
       (save-excursion
         (save-restriction
           (org-back-to-heading)
           (org-narrow-to-subtree)
           (while (re-search-forward ,reg nil t)
             (let*
                 ((org-export-preserve-breaks t)
                  (front-string (match-string-no-properties ,front))
                  (back-string (if ,back
                                   (match-string-no-properties ,back)
                                 nil))
                  (front (org-anki--org-to-html (string-clean-whitespace
                                                 front-string)))
                  (maybe-id (org-entry-get nil org-anki-prop-note-id))
                  (back (if ,back
                            (org-anki--org-to-html
                             (string-clean-whitespace
                              back-string))
                          ""))
                  (tags (org-anki--get-tags))
                  (note-start (point))
                  (card (make-org-anki--note
                         :maybe-id (if (stringp maybe-id)
                                       (string-to-number maybe-id))
						 :fields   (list (cons "Back" back)
										 (cons "Front" front))
                         :tags     tags
                         :deck     deck
                         :type     type
                         :point    note-start)))
               (eli/org-anki-sync-item card))))))))

(eli/org-anki-install "description" (rx bol
                                        (* " ")
                                        "- "
                                        (group (* any))
                                        " :: "
                                        (group (* any)
                                               (? "\n")
                                               (* (** 1 2 " ")
                                                  (* any)
                                                  (? "\n")))) 1 2)

(eli/org-anki-install "checkbox" "^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\| \\|\\([0-9]+\\)/\\1\\)\\] \\(.*\n?\\(?: \\{1,2\\}.*\n?\\)*\\)" 2)

;;;###autoload
(defun org-anki-sync-region (beg end)
  (interactive "r")
  (let* ((org-export-preserve-breaks t)
         (text (buffer-substring beg end))
         (regexp (read-regexp "Input a Regexp: "))
         (_void (string-match regexp
                              text))
         (front-string (match-string-no-properties 1 text))
         (back-string (match-string-no-properties 2 text))
         (front (org-anki--org-to-html
                 (read-string "Front Card: "
                              (string-clean-whitespace front-string))))
         (back (org-anki--org-to-html
                (read-string "Back Card: "
                             (if (member (prefix-numeric-value
                                          current-prefix-arg)
                                         '(4 16 64))
                                 (string-clean-whitespace back-string)
                               back-string))))
         (deck (read-string "Input Deck to import: "))
         (type (org-anki--find-prop
                org-anki-note-type org-anki-default-note-type))
         (note-start (point))
         (maybe-id (org-entry-get nil org-anki-prop-note-id))
         (tags (org-anki--get-tags))
         (card (make-org-anki--note
                :maybe-id (if (stringp maybe-id)
                              (string-to-number maybe-id))
                :fields   (list (cons "Back" back)
								(cons "Front" front))
                :tags     tags
                :deck     deck
                :type     type
                :point    note-start)))
    (eli/org-anki-sync-item card)
    (deactivate-mark)))

(defalias 'org-anki-sync-word
  (kmacro "C-u <f12> C-s-k \\(.*\\)\\(?:：\\|:\\)\\(\\(?:.* C-q C-j ?\\)*\\) <return> SPC 的语境 <return> <return> Words <return>"))

(defalias 'org-anki-sync-poem
  (kmacro "<f12> \\(.*\\) C-q C-j C-q C-j \\(\\(?:.* C-q C-j ?\\)*\\) <return> <return> <return> Poems <return>"))

;;;; provide
(provide 'lib-org-anki)
;;; lib-org-anki.el ends here.
