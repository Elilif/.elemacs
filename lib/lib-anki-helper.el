;;; -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

;; SPDX-License-Identifier: GPL-3.0-or-later

(defun anki-helper--entry-locate (filename entry-name)
  "Locate the entry of current Anki card."
  (find-file filename)
  (let* ((data (org-element-parse-buffer))
         (pos (org-element-map data '(headline)
                (lambda (elt)
                  (when (string= (org-element-property :raw-value elt)
                                 entry-name)
                    (org-element-property :begin elt)))
                nil t)))
    (goto-char pos)
    (org-reveal)))

(defun anki-helper-fields-get-with-backlink ()
  "Get filed info of the current entry with backlink."
  (let* ((front-and-back (anki-helper-fields-get-default))
         (filename (file-name-nondirectory (buffer-file-name)))
         (elt (plist-get (org-element-at-point) 'headline))
         (entry (plist-get elt :raw-value)))
    `(,@front-and-back ,filename ,entry)))

(defun anki-helper-cloze-fields-get-with-backlink ()
  "Get filed info of the current entry with backlink."
  (let* ((front-and-back (anki-helper-fields-get-cloze))
         (filename (file-name-nondirectory (buffer-file-name)))
         (elt (plist-get (org-element-at-point) 'headline))
         (entry (plist-get elt :raw-value)))
    `(,@front-and-back ,filename ,entry)))

(defun eli/anki-helper-snyc-items (reg nums)
  "Snyc org itmes(e.g. plain lists and description lists).

REG is the regexp of the item, NUMS is a list of numbers
specifying which parenthesized expressions in the last regexp,
see `match-string-no-properties' for details."
  (save-excursion
    (org-back-to-heading)
    (let ((end (org-entry-end-position))
          notes)
      (while (re-search-forward reg end t)
        (let* ((contents (mapcar (lambda (num)
                                   (match-string-no-properties num))
                                 nums)))
          (push (anki-helper-create-note contents) notes)))
      (anki-helper-request 'addNotes (anki-helper-create-notes notes)))))

;;;###autoload
(defun eli/anki-helper-snyc-checkbox ()
  "Snyc checkboxes."
  (interactive)
  (eli/anki-helper-snyc-items "^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\| \\|\\([0-9]+\\)/\\1\\)\\] \\(.*\n?\\(?: \\{1,2\\}.*\n?\\)*\\)" '(2)))

;;;###autoload
(defun eli/anki-helper-sync-description-lists ()
  "Sync description lists."
  (interactive)
  (eli/anki-helper-snyc-items (rx bol
                                  (* " ")
                                  "- "
                                  (group (* any))
                                  " :: "
                                  (group (* any)
                                         (? "\n")
                                         (* (** 1 2 " ")
                                            (* any)
                                            (? "\n"))))
                              '(1 2)))

(defun eli/org-headline-empty-p ()
  "Return non-nil if current headline is empty."
  (not (org-element-property :robust-begin (org-element-at-point))))

(defun eli/anki-helper-skip ()
  "Skip headlines with \"noanki\" property or with `org-anki-prop-note-id'.
Used by `anki-helper-skip-function'"
  (if (or (string= "t" (org-entry-get nil "NOANKI"))
          (eli/org-headline-empty-p))
      (point)))

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

(provide 'lib-anki-helper)
;;; lib-anki-helper.el ends here
