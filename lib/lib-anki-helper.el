;;; -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

;; SPDX-License-Identifier: GPL-3.0-or-later

(defun anki-helper--entry-locate (_filename id)
  "Locate the entry of current Anki card."
  (org-id-goto id))

(defun anki-helper-fields-get-with-backlink ()
  "Get filed info of the current entry with backlink."
  (let* ((front-and-back (anki-helper-fields-get-default))
         (filename (file-name-nondirectory (buffer-file-name)))
         (id (org-id-get-create)))
    `(,@front-and-back ,filename ,id)))

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

;;;###autoload
(defun eli/anki-helper-snyc-poem (beg end)
  (interactive "r")
  (let* ((anki-helper-default-deck "Poems")
         (anki-helper-default-note-type "Poems")
         (back (buffer-substring-no-properties beg end))
         (anki-helper-default-tags
          (progn
            (string-match "\\[\\(.*?\\)\\] *\\(.*?\\)\n" back)
            (list (match-string 1 back)
                  (match-string 2 back)))))
    (anki-helper-make-two-sided-card
     beg end
     (lambda (text)
       (format
        "<a href=https://so.gushiwen.cn/search.aspx?value=%s&valuej=%s>%s</a>"
        (url-encode-url text)
        (url-encode-url (substring text 0 1))
        text)))))

;;;###autoload
(defun eli/anki-helper-snyc-pingshuiyun ()
  (interactive)
  (when (region-active-p)
    (copy-rectangle-as-kill (region-beginning) (region-end)))
  (let (notes)
    (dolist (char killed-rectangle)
      (let* ((result (souyun-query-char char))
             (anki-helper-default-note-type "Basic")
             (anki-helper-default-deck "平水韵")
             (anki-helper-default-tags (list result)))
        (push (anki-helper-create-note (list char result)) notes)))
    (anki-helper-request 'addNotes (anki-helper-create-notes notes))))

;;;###autoload
(defun eli/anki-helper-set-deck (deck)
  "Set the deck for the current entry."
  (interactive "MDeck Name: ")
  (org-set-property anki-helper-prop-deck deck))

;;;###autoload
(defun eli/anki-helper-set-note-type (note-type)
  "Set the note type for the current entry."
  (interactive (list (completing-read "Select: " anki-helper-note-types)))
  (org-set-property anki-helper-note-type note-type))

(defun eli/org-headline-empty-p ()
  "Return non-nil if current headline is empty."
  (not (or (org-element-property :robust-begin (org-element-at-point))
           (org-element-property :contents-begin (org-element-at-point)))))

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
