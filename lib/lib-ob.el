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

(defmacro eli/org-src-ref-expand (&rest body)
  `(let* ((split-file (match-string 1 id))
          (split-ref (match-string 2 id))
          (regexp (org-babel-named-src-block-regexp-for-name split-ref)))
     (save-window-excursion
       (find-file split-file)
       (org-with-wide-buffer
        (goto-char (point-min))
        (if (re-search-forward regexp nil t)
            (unless (org-in-commented-heading-p)
              (let ((e (org-element-at-point)))
                (when (equal (org-element-property :name e) split-ref)
                  (goto-char
                   (org-element-property :post-affiliated e))
                  ,@body)))
          "")))))

(defun eli/org-babel-expand-noweb-references (&optional info parent-buffer)
  "Advice for `org-babel-expand-noweb-references'.

Add some text properties to expanded noweb references"
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
      (setq-local org-babel-expand-noweb-references--cache nil
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
                           (setq-local org-babel-expand-noweb-references--cache nil
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
                                    (setq-local org-babel-expand-noweb-references--cache (make-hash-table :test #'equal)))
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
                      ((string-match "^\\(.+\\):\\(.+\\)$" id)
                       (eli/org-src-ref-expand
                        (eli/org-babel-expand-noweb-references)))
                      ;; Though luck.  We go into the long process of
                      ;; checking each source block and expand those
                      ;; with a matching Noweb reference.  Since we're
                      ;; going to visit all source blocks in the
                      ;; document, cache information about them as well.
                      (t
                       (setq-local org-babel-expand-noweb-references--cache (make-hash-table :test #'equal))
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
                                expansion))
                      (name (if (string-match "\\(.*?\\)(.*?)" id)
                                (match-string 1 id)
                              id)))
                 (font-lock-prepend-text-property 0 (length result) 'cnoweb name result)
                 (propertize result 'noweb id))))))
       body t t 2))))

(defun eli/org-src-add-overlays ()
  "Highlight expanded text."
  (remove-hook 'org-src-mode-hook #'eli/org-src-add-overlays)
  (setq-local org-src--allow-write-back
              (lambda ()
                (org-escape-code-in-region (point-min) (point-max))))
  (save-excursion
    (with-silent-modifications
      (indent-region (point-min) (point-max))
      (goto-char (point-min))
      (save-excursion
        (if-let* ((orig (text-property-search-forward 'orig))
                  (beg (prop-match-beginning orig))
                  (end (prop-match-end orig)))
            ;; (goto-char beg)
            ;; (unless (bolp)
            ;;   (setq beg (1- beg)))
            (progn
              (goto-char end)
              (when (and (eolp)
                         (not (eobp)))
                (setq end (1+ end)))
              (put-text-property (point-min) beg 'expanded t)
              (put-text-property end (point-max) 'expanded t))
          (unless (buffer-narrowed-p)
            (put-text-property (point-min) (point-max) 'expanded t))))

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
  "Remove expanded text."
  (goto-char (point-min))
  (let* ((inhibit-read-only t)
         prop)
    (while (setq prop (text-property-search-forward 'expanded))
      (delete-region (prop-match-beginning prop) (prop-match-end prop)))
    (goto-char (point-min))
    (while (setq prop (text-property-search-forward 'noweb))
      (delete-region (prop-match-beginning prop)
                     (prop-match-end prop))
      (insert (concat
               org-babel-noweb-wrap-start
               (prop-match-value prop)
               org-babel-noweb-wrap-end)))
    (indent-region (point-min) (point-max))))

(defun eli/org-src-save-around (orig-fun)
  (if (or (text-property-any (point-min) (point-max) 'expanded t)
          (text-property-any (point-min) (point-max) 'orig t))
      (let* ((orig-pos (point))
             (inhibit-read-only t)
             (beg 1)
             string expanded)
        (goto-char (point-min))
        (while (setq expanded (text-property-search-forward 'expanded))
          (let* ((prop-beg (prop-match-beginning expanded))
                 (prop-end (prop-match-end expanded)))
            (unless (<= prop-beg beg)
              (put-text-property beg prop-beg 'orig t))
            (setq beg prop-end)))
        (put-text-property (point) (point-max) 'orig t)
        (setq string (buffer-substring (point-min) (point-max)))
        (eli/org-src-clean)
        (funcall orig-fun)
        (delete-region (point-min) (point-max))
        (insert string)
        (eli/org-src-add-overlays)
        (set-buffer-modified-p nil)
        (goto-char orig-pos))
    (funcall orig-fun)))

(defun eli/org-babel-expand-src-block (info &optional params)
  (let* ((lang (nth 0 info))
         (params (setf (nth 2 info)
                       (sort (org-babel-merge-params (nth 2 info) params)
                             (lambda (el1 el2) (string< (symbol-name (car el1))
                                                        (symbol-name (car el2)))))))
         (contents (if (org-babel-noweb-p params :eval)
                       (org-babel-expand-noweb-references info)
                     (nth 1 info)))
         (body (setf (nth 1 info)
                     (propertize contents 'orig t)))
         (expand-cmd (intern (concat "org-babel-expand-body:" lang)))
         (assignments-cmd (intern (concat "org-babel-variable-assignments:"
                                          lang))))
    (if (fboundp expand-cmd)
        (funcall expand-cmd body params)
      (org-babel-expand-body:generic
       body params (and (fboundp assignments-cmd)
                        (funcall assignments-cmd params))))))

;;;###autoload
(defun eli/org-babel-expand-src-block-and-edit (&optional info params)
  "Expand the current source code block."
  (interactive)
  (if (eq (org-element-type (org-element-at-point)) 'src-block)
      (let* ((info (or info (org-babel-get-src-block-info)))
             (name (nth 4 info))
             (expanded (eli/org-babel-expand-src-block info params))
             (buffer (org-src--construct-edit-buffer-name (buffer-name) name)))
        (add-hook 'org-src-mode-hook #'eli/org-src-add-overlays -100)
        (org-edit-src-code expanded buffer)
        (with-current-buffer buffer
          (if-let ((eob (point-max))
                   (pos (text-property-any (point-min) (point-max) 'read-only nil)))
              (goto-char (min (1+ pos) eob))
            (goto-char eob))))
    (user-error "No src block to edit here")))

;;;###autoload
(defun eli/org-src-noweb-expand (&optional string)
  "Expand noweb reference before point."
  (interactive)
  (let* ((info (seq-copy org-src--babel-info))
         (beg (line-beginning-position))
         (end (point))
         (string (or string (buffer-substring-no-properties beg end)))
         (result))
    (setf (nth 1 info) string)
    (setq result (eli/org-babel-expand-noweb-references
                  info (org-src-source-buffer)))
    (save-restriction
      (narrow-to-region beg end)
      (delete-region beg end)
      (insert result)
      (eli/org-src-add-overlays))))

;;;###autoload
(defun eli/org-src-delete ()
  "Delete noweb reference under point."
  (interactive)
  (let* ((ov (cl-find-if (lambda (ov)
                           (overlay-get ov 'evaporate))
                         (overlays-at (point))))
         (inhibit-read-only t))
    (delete-region (overlay-start ov) (overlay-end ov))))

(defvar eli/org-src-noweb-history nil)

;;;###autoload
(defun eli/org-src-noweb-jump ()
  "Edit current noweb under point."
  (interactive)
  (when org-src-mode
    (let* ((ref? (lambda (target) (string-match "^\\(.+\\):\\(.+\\)$" target)))
           (nowebs (get-text-property (point) 'cnoweb))
           (other-file? (cl-find-if ref? nowebs))
           (file (and other-file? (match-string 1 other-file?)))
           (last (car (last nowebs))))
      (when-let* ((id (if (and file
                               (not (funcall ref? last)))
                          (concat file ":" last)
                        last))
                  (pos (or (and (funcall ref? id)
                                (eli/org-src-ref-expand
                                 (copy-marker (point))))
                           (nth 5 (car-safe (gethash id  (org-src-do-at-code-block
                                                          org-babel-expand-noweb-references--cache)))))))
        (org-edit-src-exit)
        (push (copy-marker (point)) eli/org-src-noweb-history)
        (when (markerp pos)
          (switch-to-buffer (marker-buffer pos)))
        (goto-char pos)
        (eli/org-babel-expand-src-block-and-edit)
        (run-with-timer 0.1 nil (lambda () (lsp)))))))

;;;###autoload
(defun eli/org-src-noweb-back ()
  (interactive)
  (when org-src-mode
    (org-edit-src-exit))
  (when-let ((marker (pop eli/org-src-noweb-history)))
    (switch-to-buffer (marker-buffer marker))
    (goto-char marker)
    (eli/org-babel-expand-src-block-and-edit)
    (run-with-timer 0.1 nil (lambda () (lsp)))))

;;;; coderef
(defun eli/org-src-set-coderef-label-format ()
  (setq-local org-coderef-label-format
              (concat (comment-padright comment-start comment-add)
                      org-coderef-label-format)))

(provide 'lib-ob)
;;; lib-ob.el ends here