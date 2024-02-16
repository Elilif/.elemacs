;;; -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'anki-helper)

(defvar killed-rectangle)

(defun anki-helper-fields-get-cloze-dwim ()
  "Default function for get filed info of the current entry for
\"Cloze\" note-type."
  (let* ((fields (cond
                  ((get-text-property (point) 'anki-helper--note)
                   (let* ((prop (save-excursion
                                  (text-property-search-forward
                                   'anki-helper--note t t)
                                  (text-property-search-backward
                                   'anki-helper--note t t)))
                          (string (buffer-substring-no-properties
                                   (prop-match-beginning prop)
                                   (prop-match-end prop))))
                     (list string "")))
                  ((region-active-p)
                   (let ((string (buffer-substring-no-properties
                                  (region-beginning) (region-end))))
                     (list string "")))
                  (t (let* ((pair (anki-helper-fields-get-default))
                            (back (cadr pair)))
                       (list back (car pair))))))
         (text (car fields)))
    (setf (car fields) (if anki-helper-cloze-use-emphasis
                           (anki-helper--make-cloze text)
                         text))
    fields))

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
         (back (replace-regexp-in-string
                "\\(\n\\).+" "<br>"
                (buffer-substring-no-properties beg end)
                nil nil 1)))
    (let* ((link (elfeed-entry-link elfeed-show-entry))
           (title (elfeed-entry-title elfeed-show-entry))
           (meta (elfeed-entry-meta elfeed-show-entry))
           (author (car (alist-get :email (plist-get meta :authors))))
           (anki-helper-default-tags
            (progn
              (string-match "\\(.*?\\)〔\\(.*?\\)〕" author)
              (list (match-string 1 author)
                    (match-string 2 author)))))
      (anki-helper-request 'addNote (anki-helper-create-note
                                     (list (format "<a href=%s>%s</a>"
                                                   link title)
                                           (concat author "<br>" back)))))))

;;;###autoload
(defun eli/anki-helper-snyc-pingshuiyun ()
  (interactive)
  (when (region-active-p)
    (copy-rectangle-as-kill (region-beginning) (region-end)))
  (let (notes)
    (dolist (char killed-rectangle)
      (let ((result (souyun-query-char char)))
        (push (anki-helper-create-note (list char result)
                                       :tags (list result)
                                       :deck "平水韵"
                                       :model "Basic")
              notes)))
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


;;;; anki-helper region
(defface anki-helper-ov
  '((t (:bold t :inherit shadow)))
  "Marks an anki note."
  :group 'anki-helper)

(defun anki-helper--get-bounds ()
  "Return the anki card boundaries as an alist."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (let ((prop) (hash) (id) (note-type) (bounds))
        (while (setq prop (text-property-search-backward
                           'anki-helper--note t t))
          (setq hash (get-text-property (point) 'anki-helper--hash)
                id (get-text-property (point) 'anki-helper--id)
                note-type (get-text-property (point) 'anki-helper--note-type))
          (push (list (prop-match-beginning prop) (prop-match-end prop)
                      hash note-type id)
                bounds))
        bounds))))

;;;###autoload
(defun anki-helper--save-sate ()
  "Write anki card boundaries to the buffer.

This enables saving anki card boundaries when writing the buffer
to disk."
  (when (and (derived-mode-p 'org-mode)
             (or (org-entry-get (point-min) "ANKI_HELPER_BOUNDS")
                 (anki-helper--get-bounds)))
    (letrec ((write-bounds
              (lambda (attempts)
                (let* ((bounds (anki-helper--get-bounds))
                       (offset (caar bounds))
                       (offset-marker (set-marker (make-marker) offset)))
                  (org-entry-put (point-min) "ANKI_HELPER_BOUNDS"
                                 (prin1-to-string bounds))
                  (when (and offset
                             (not (= (marker-position offset-marker) offset))
                             (> attempts 0))
                    (funcall write-bounds (1- attempts)))))))
      (funcall write-bounds 6))))

;;;###autoload
(defun anki-helper-restore-state ()
  (when (buffer-file-name)
    (save-restriction
      (condition-case-unless-debug nil
          (progn
            (when-let ((bounds (org-entry-get (point-min) "ANKI_HELPER_BOUNDS")))
              (mapc (pcase-lambda (`(,beg ,end ,hash ,note-type ,id))
                      (unless (get-text-property beg 'anki-helper--note)
                        (anki-helper-region-add-info beg end hash note-type id)))
                    (read bounds))
              (message "anki notes restored.")))
        (error (message "Could not restore anki notes, sorry!"))))))

(defun anki-helper-region-add-info (beg end hash note-type &optional id)
  (let ((ov (make-overlay beg end)))
    (put-text-property beg end 'anki-helper--note t)
    (put-text-property beg end 'anki-helper--note-type note-type)
    (put-text-property beg end 'front-sticky t)
    (put-text-property beg end 'anki-helper--hash hash)
    (when id
      (put-text-property beg end 'anki-helper--id id))
    (overlay-put ov
                 'before-string
                 (propertize
                  "{{" 'face 'anki-helper-ov))
    (overlay-put ov
                 'after-string
                 (propertize
                  "}}" 'face 'anki-helper-ov))))

;;;###autoload
(defun anki-helper-region (beg end)
  "Create an inline note for selected region."
  (interactive (anki-helper--get-region))
  (unless (get-text-property beg 'anki-helper--note)
    (let* ((string (buffer-substring-no-properties beg end))
           (note-type (if current-prefix-arg
                          (completing-read "Select a note type: " anki-helper-note-types)
                        "Cloze"))
           (contents (funcall (alist-get
                               note-type
                               anki-helper-fields-get-alist
                               #'anki-helper-fields-get-default
                               nil #'string=)))
           (hash (md5 string)))
      (anki-helper-request 'addNote
                           (anki-helper-create-note (mapcar
                                                     #'anki-helper--org2html
                                                     contents)
                                                    :model note-type)
                           (list :command 'anki-helper-region
                                 :orig-info `(:pos ,(point-marker) :beg ,beg :end ,end)))
      (anki-helper-region-add-info beg end hash note-type))))

(defun anki-helper-region-callback (info result)
  (if-let ((marker (plist-get info :pos))
           (beg (plist-get info :beg))
           (end (plist-get info :end))
           (id result))
      (save-excursion
        (with-current-buffer (marker-buffer marker)
          (put-text-property beg end 'anki-helper--id id))))
  (message "Synchronizing cards...done."))

(add-to-list 'anki-helper-callback-alist
             '(anki-helper-region . anki-helper-region-callback))

(defun anki-helper--get-region ()
  (cond
   ((region-active-p)
    (list (copy-marker (region-beginning)) (copy-marker (region-end))))
   ((get-text-property (point) 'anki-helper--note)
    (let* ((prop (save-excursion
                   (text-property-search-forward
                    'anki-helper--note t t)
                   (text-property-search-backward
                    'anki-helper--note t t))))
      (list (copy-marker (prop-match-beginning prop))
            (copy-marker (prop-match-end prop)))))
   (t (user-error "No anki note found!"))))

;;;###autoload
(defun anki-helper-region-delete (beg end)
  (interactive (anki-helper--get-region))
  (let ((id (get-text-property beg 'anki-helper--id))
        (ovs (overlays-in beg end)))
    (when id
      (anki-helper-request 'deleteNotes
                           (list id)))
    (dolist (prop '(anki-helper--note
                    anki-helper--hash
                    anki-helper--id
                    front-sticky))
      (remove-text-properties beg end `(,prop nil)))
    (dolist (ov ovs)
      (delete-overlay ov))))

(dolist (prop '(anki-helper--note
                anki-helper--hash
                anki-helper--id
                anki-helper--note-type
                front-sticky))
  (add-to-list 'yank-excluded-properties prop))

(defun anki-helper--region-collect (&optional force)
  "Create a `anki-helper--note' struct for current Anki entry."
  (save-excursion
    (let ((prop) (notes))
      (goto-char (point-max))
      (while (setq prop (text-property-search-backward
                         'anki-helper--note t t))
        (when-let* ((beg (prop-match-beginning prop))
                    (end (prop-match-end prop))
                    (string (buffer-substring-no-properties beg end))
                    (old-hash (get-text-property (point) 'anki-helper--hash))
                    (new-hash (md5 string))
                    (change-p (or force
                                  (not (string= old-hash new-hash)))))
          (let* ((pos (point))
                 (note-type (get-text-property pos 'anki-helper--note-type))
                 (fields (anki-helper--entry-get-fields note-type))
                 (id (get-text-property pos 'anki-helper--id))
                 (deck (anki-helper--find-prop
                        anki-helper-prop-deck
                        anki-helper-default-deck))
                 (tags (anki-helper--get-tags))
                 (hash (md5 (format "%s%s"
                                    (random)
                                    (anki-helper--filelds2string fields "")))))
            (push (make-anki-helper--note
                   :maybe-id id
                   :deck deck
                   :fields fields
                   :tags tags
                   :model note-type
                   :orig-pos (list :pos (point-marker)
                                   :beg beg
                                   :end end)
                   :hash hash)
                  notes))))
      (let ((positions (mapcar (lambda (note)
                                 (anki-helper--note-orig-pos note))
                               notes)))
        (cons (anki-helper--transform-notes notes) positions)))))

;;;###autoload
(defun anki-helper-region-update-all (&optional force)
  (interactive "p")
  (if-let* ((result (anki-helper--region-collect force))
            (body (mapcar #'anki-helper--action-updatenote (car result))))
      (anki-helper-request 'multi
                           body
                           (list :command 'anki-helper-region-update-all
                                 :orig-info (cdr result)))
    (message "anki-helper: no update needed.")))

(defun anki-helper-region-update-callback (info _result)
  (dolist (note info)
    (if-let ((marker (plist-get note :pos))
             (beg (plist-get note :beg))
             (end (plist-get note :end)))
        (save-excursion
          (with-current-buffer (marker-buffer marker)
            (put-text-property beg end 'anki-helper--hash
                               (md5 (buffer-substring-no-properties
                                     beg end)))))))
  (message "Updating cards...done"))

(add-to-list 'anki-helper-callback-alist
             '(anki-helper-region-update-all . anki-helper-region-update-callback))

(provide 'lib-anki-helper)
;;; lib-anki-helper.el ends here
