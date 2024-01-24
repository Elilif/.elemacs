;;; lib-ob.el --- orb babel config -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

(defface org-src-read-only
  '((t (:background "#e8e8e8" :extend t)))
  "Face for read only text in `org-src-mode'")

(defvar eli/org-src-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "s-d" #'eli/org-src-delete)
    map)
  "Keymap automatically activated inside overlays.
You can re-bind the commands to any keys you prefer.")


(defun eli/org-babel-expand-noweb-references (&optional info parent-buffer)
  "Advice for `org-babel-expand-noweb-references'.

Add some text properties to expaned noweb references"
  (let* ((parent-buffer (or parent-buffer (current-buffer)))
         (info (or info (org-babel-get-src-block-info 'no-eval)))
         (lang (nth 0 info))
         (body (nth 1 info))
         (comment (string= "noweb" (cdr (assq :comments (nth 2 info)))))
         (noweb-prefix (let ((v (assq :noweb-prefix (nth 2 info))))
                         (or (not v)
                             (and (org-not-nil (cdr v))
                                  (not (equal (cdr v) "no"))))))
         (noweb-re (format "\\(.*?\\)\\(%s\\)"
                           (with-current-buffer parent-buffer
                             (org-babel-noweb-wrap)))))
    (unless (equal (cons parent-buffer
                         (with-current-buffer parent-buffer
                           (buffer-chars-modified-tick)))
                   org-babel-expand-noweb-references--cache-buffer)
      (setq org-babel-expand-noweb-references--cache nil
            org-babel-expand-noweb-references--cache-buffer
            (cons parent-buffer
                  (with-current-buffer parent-buffer
                    (buffer-chars-modified-tick)))))
    (cl-macrolet ((c-wrap
                    (s)
                    ;; Comment string S, according to LANG mode.  Return new
                    ;; string.
                    `(unless org-babel-tangle-uncomment-comments
                       (with-temp-buffer
                         (funcall (org-src-get-lang-mode lang))
                         (comment-region (point)
                                         (progn (insert ,s) (point)))
                         (org-trim (buffer-string)))))
                  (expand-body
                    (i)
                    ;; Expand body of code represented by block info I.
                    `(let ((b (if (org-babel-noweb-p (nth 2 ,i) :eval)
                                  (org-babel-expand-noweb-references ,i)
                                (nth 1 ,i))))
                       (if (not comment) b
                         (let ((cs (org-babel-tangle-comment-links ,i)))
                           (concat (c-wrap (car cs)) "\n"
                                   b "\n"
                                   (c-wrap (cadr cs)))))))
                  (expand-references
                    (ref)
                    `(pcase (gethash ,ref org-babel-expand-noweb-references--cache)
                       (`(,last . ,previous)
                        ;; Ignore separator for last block.
                        (let ((strings (list (expand-body last))))
                          (dolist (i previous)
                            (let ((parameters (nth 2 i)))
                              ;; Since we're operating in reverse order, first
                              ;; push separator, then body.
                              (push (or (cdr (assq :noweb-sep parameters)) "\n")
                                    strings)
                              (push (expand-body i) strings)))
                          (mapconcat #'identity strings "")))
                       ;; Raise an error about missing reference, or return the
                       ;; empty string.
                       ((guard (or org-babel-noweb-error-all-langs
                                   (member lang org-babel-noweb-error-langs)))
                        (error "Cannot resolve %s (see `org-babel-noweb-error-langs')"
                               (org-babel-noweb-wrap ,ref)))
                       (_ ""))))
      (replace-regexp-in-string
       noweb-re
       (lambda (m)
         (with-current-buffer parent-buffer
           (save-match-data
             (let* ((prefix (match-string 1 m))
                    (id (match-string 3 m))
                    (evaluate (string-match-p "(.*)" id))
                    (expansion
                     (cond
                      (evaluate
                       (prog1
                           (let ((raw (org-babel-ref-resolve id)))
                             (if (stringp raw) raw (format "%S" raw)))
                         ;; Evaluation can potentially modify the buffer
                         ;; and invalidate the cache: reset it.
                         (unless (equal org-babel-expand-noweb-references--cache-buffer
                                        (cons parent-buffer
                                              (buffer-chars-modified-tick)))
                           (setq org-babel-expand-noweb-references--cache nil
                                 org-babel-expand-noweb-references--cache-buffer
                                 (cons parent-buffer
                                       (with-current-buffer parent-buffer
                                         (buffer-chars-modified-tick)))))))
                      ;; Already cached.
                      ((and (hash-table-p org-babel-expand-noweb-references--cache)
                            (gethash id org-babel-expand-noweb-references--cache))
                       (expand-references id))
                      ;; Return the contents of headlines literally.
                      ((org-babel-ref-goto-headline-id id)
                       (org-babel-ref-headline-body))
                      ;; Look for a source block named SOURCE-NAME.  If
                      ;; found, assume it is unique; do not look after
                      ;; `:noweb-ref' header argument.
                      ((org-with-point-at 1
                         (let ((r (org-babel-named-src-block-regexp-for-name id)))
                           (and (re-search-forward r nil t)
                                (not (org-in-commented-heading-p))
                                (let ((info (org-babel-get-src-block-info t)))
                                  (unless (hash-table-p org-babel-expand-noweb-references--cache)
                                    (setq org-babel-expand-noweb-references--cache (make-hash-table :test #'equal)))
                                  (push info (gethash id  org-babel-expand-noweb-references--cache))
                                  (expand-body info))))))
                      ;; Retrieve from the Library of Babel.
                      ((nth 2 (assoc-string id org-babel-library-of-babel)))
                      ;; All Noweb references were cached in a previous
                      ;; run.  Yet, ID is not in cache (see the above
                      ;; condition).  Process missing reference in
                      ;; `expand-references'.
                      ((and (hash-table-p org-babel-expand-noweb-references--cache)
                            (gethash 'buffer-processed org-babel-expand-noweb-references--cache))
                       (expand-references id))
                      ;; Though luck.  We go into the long process of
                      ;; checking each source block and expand those
                      ;; with a matching Noweb reference.  Since we're
                      ;; going to visit all source blocks in the
                      ;; document, cache information about them as well.
                      (t
                       (setq org-babel-expand-noweb-references--cache (make-hash-table :test #'equal))
                       (org-with-wide-buffer
                        (org-babel-map-src-blocks nil
                          (if (org-in-commented-heading-p)
                              (org-forward-heading-same-level nil t)
                            (let* ((info (org-babel-get-src-block-info t))
                                   (ref (cdr (assq :noweb-ref (nth 2 info)))))
                              (push info (gethash ref org-babel-expand-noweb-references--cache))))))
                       (puthash 'buffer-processed t org-babel-expand-noweb-references--cache)
                       (expand-references id)))))
               ;; Interpose PREFIX between every line.
               (let* ((result (if noweb-prefix
                                  (mapconcat #'identity
                                             (split-string expansion "[\n\r]")
                                             (concat "\n" prefix))
                                expansion)))
                 (propertize result
                             'noweb (concat
                                     org-babel-noweb-wrap-start
                                     id
                                     org-babel-noweb-wrap-end)))))))
       body t t 2))))

(defun eli/org-src-add-overlays ()
  "Highlight expaned text."
  (read-only-mode -1)
  (setq-local org-src--allow-write-back
              (lambda ()
                (org-escape-code-in-region (point-min) (point-max))))
  (save-excursion
    (with-silent-modifications
      (indent-region (point-min) (point-max))
      (goto-char (point-min))
      (save-excursion
        (when-let* ((orig (text-property-search-forward 'orig))
                    (beg (prop-match-beginning orig))
                    (end (prop-match-end orig)))
          ;; (goto-char beg)
          ;; (unless (bolp)
          ;;   (setq beg (1- beg)))
          (goto-char end)
          (when (eolp)
            (setq end (1+ end)))
          (put-text-property (point-min) beg 'expanded t)
          (put-text-property end (point-max) 'expanded t)))

      (dolist (target '(expanded noweb))
        (let (prop)
          (save-excursion
            (while (setq prop (text-property-search-forward target))
              (let* ((beg (prop-match-beginning prop))
                     (end (prop-match-end prop))
                     (ov (make-overlay beg end)))
                (overlay-put ov 'face 'org-src-read-only)
                (overlay-put ov 'evaporate t)
                (overlay-put ov 'keymap eli/org-src-map)
                (dolist (prop '(read-only))
                  (put-text-property beg end prop t))
                (put-text-property (max (point-min) (1- beg)) beg 'rear-nonsticky t)
                (put-text-property (1- end) end 'rear-nonsticky t)))))))))

(defun eli/org-src-clean (&rest _args)
  "Remove expaned text."
  (save-excursion
    (goto-char (point-min))
    (let* ((inhibit-read-only t)
           prop)
      (while (setq prop (text-property-search-forward 'expanded))
        (delete-region (prop-match-beginning prop) (prop-match-end prop)))
      (goto-char (point-min))
      (while (setq prop (text-property-search-forward 'noweb))
        (delete-region (prop-match-beginning prop)
                       (prop-match-end prop))
        (insert (prop-match-value prop)))
      (indent-region (point-min) (point-max)))))

;;;###autoload
(defun eli/org-babel-expand-src-block-and-edit (&optional _arg info params)
  "Expand the current source code block."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (nth 0 info))
         (params (setf (nth 2 info)
                       (sort (org-babel-merge-params (nth 2 info) params)
                             (lambda (el1 el2) (string< (symbol-name (car el1))
                                                        (symbol-name (car el2)))))))
         (contents (if (org-babel-noweb-p params :eval)
                       (org-babel-expand-noweb-references info)
                     (nth 1 info)))
         (body (setf (nth 1 info)
                     (propertize (if (string-empty-p contents) "\n" contents)
                                 'orig t)))
         (expand-cmd (intern (concat "org-babel-expand-body:" lang)))
         (assignments-cmd (intern (concat "org-babel-variable-assignments:"
                                          lang)))
         (expanded
          (if (fboundp expand-cmd)
              (funcall expand-cmd body params)
            (org-babel-expand-body:generic
             body params (and (fboundp assignments-cmd)
                              (funcall assignments-cmd params)))))
         (buffer (org-src--construct-edit-buffer-name (buffer-name) lang)))
    (org-edit-src-code expanded buffer)
    (with-current-buffer buffer
      (when-let ((prop (text-property-search-forward 'orig)))
        (goto-char (prop-match-beginning prop))))))

;;;###autoload
(defun eli/org-src-noweb-expand ()
  "Expand noweb reference before point."
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (point))
         (ref (buffer-substring-no-properties beg end))
         (parent-buffer (overlay-buffer org-src--overlay))
         (result))
    (setf (nth 1 org-src--babel-info) ref)
    (setq result (eli/org-babel-expand-noweb-references
                  org-src--babel-info parent-buffer))
    (save-window-excursion
      (save-restriction
        (narrow-to-region beg end)
        (delete-region beg end)
        (insert result)
        (eli/org-src-add-overlays)))))

;;;###autoload
(defun eli/org-src-delete ()
  "Delete noweb reference under point."
  (interactive)
  (let* ((ov (cl-find-if (lambda (ov)
                           (overlay-get ov 'evaporate))
                         (overlays-at (point))))
         (inhibit-read-only t))
    (delete-region (overlay-start ov) (overlay-end ov))))

(provide 'lib-ob)
;;; lib-ob.el ends here
