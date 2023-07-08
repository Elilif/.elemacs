;; lib-org.el --- Initialize lib-org configurations.	-*- lexical-binding: t; -*-

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

;; prevent org emphases from being split by `fill-paragraph'.
(defun eli/adjust-line-break-point (linebeg)
  (let* ((re "\\([-[:space:]('\"{[:nonascii:]]\\|^\\)\\([~=*/_+]\\)\\(?:[^ ~=*/_+].*?\\|[^ ~=*/_+].*?\n.+?\\)[~=*/_+]")
         pt)
    (save-excursion
      (end-of-line)
      (while (re-search-backward re linebeg t)
        (let* ((beg (save-excursion
                      (goto-char (match-beginning 0))
                      (current-column)))
               (end (save-excursion
                      (goto-char (match-end 0))
                      (current-column))))
          (when (and (> end fill-column)
                     (> (+ beg 6) fill-column)
                     (< beg fill-column))
            (setq pt (match-beginning 0))))))
    (when pt
      (goto-char pt)
      (forward-char 2))))

(defun eli/org-element--parse-generic-emphasis (mark type)
  "Parse emphasis object at point, if any.

MARK is the delimiter string used.  TYPE is a symbol among
`bold', `code', `italic', `strike-through', `underline', and
`verbatim'.

Assume point is at first MARK."
  (save-excursion
    (let ((origin (point)))
      (unless (bolp) (forward-char -1))
      (let ((opening-re
             (rx-to-string
              `(seq (or line-start (any space ?- ?\( ?' ?\" ?\{ nonascii))
                    ,mark
                    (not space)))))
        (when (looking-at opening-re)
          (goto-char (1+ origin))
          (let ((closing-re
                 (rx-to-string
                  `(seq
                    (not space)
                    (group ,mark)
                    (or (any space ?- ?. ?, ?\; ?: ?! ?? ?' ?\" ?\) ?\} ?\\ ?\[
                             nonascii)
                        line-end)))))
            (when (re-search-forward closing-re nil t)
              (let ((closing (match-end 1)))
                (goto-char closing)
                (let* ((post-blank (skip-chars-forward " \t"))
                       (contents-begin (1+ origin))
                       (contents-end (1- closing)))
                  (list type
                        (append
                         (list :begin origin
                               :end (point)
                               :post-blank post-blank)
                         (if (memq type '(code verbatim))
                             (list :value
                                   (and (memq type '(code verbatim))
                                        (buffer-substring
                                         contents-begin contents-end)))
                           (list :contents-begin contents-begin
                                 :contents-end contents-end)))))))))))))

(defun eli/org-do-emphasis-faces (limit)
  "Run through the buffer and emphasize strings."
  (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\)"
                          (car org-emphasis-regexp-components))))
    (catch :exit
      (while (re-search-forward quick-re limit t)
        (let* ((marker (match-string 2))
               (verbatim? (member marker '("~" "="))))
          (when (save-excursion
    	          (goto-char (match-beginning 0))
    	          (and
                   ;; Do not match if preceded by org-emphasis
                   (not (save-excursion
                          (forward-char 1)
                          (get-pos-property (point) 'org-emphasis)))
                   ;; Do not match in latex fragments.
                   (not (when (functionp 'texmathp)
						  (texmathp)))
                   (not (when (functionp 'xenops-math-parse-algorithm-at-point)
                          (xenops-math-parse-algorithm-at-point)))
                   ;; Do not match in Drawer.
                   (not (org-match-line
                         "^[ 	]*:\\(\\(?:\\w\\|[-_]\\)+\\):[ 	]*"))
    	           ;; Do not match table hlines.
    	           (not (and (equal marker "+")
    		                 (org-match-line
    		                  "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
    	           ;; Do not match headline stars.  Do not consider
    	           ;; stars of a headline as closing marker for bold
    	           ;; markup either.
    	           (not (and (equal marker "*")
    		                 (save-excursion
    		                   (forward-char)
    		                   (skip-chars-backward "*")
    		                   (looking-at-p org-outline-regexp-bol))))
    	           ;; Match full emphasis markup regexp.
    	           (looking-at (if verbatim? org-verbatim-re org-emph-re))
    	           ;; Do not span over paragraph boundaries.
    	           (not (string-match-p org-element-paragraph-separate
    				                    (match-string 2)))
    	           ;; Do not span over cells in table rows.
    	           (not (and (save-match-data (org-match-line "[ \t]*|"))
    		                 (string-match-p "|" (match-string 4))))))
            (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist))
    		            (m (if org-hide-emphasis-markers 4 2)))
              (font-lock-prepend-text-property
               (match-beginning m) (match-end m) 'face face)
              (when verbatim?
    	        (org-remove-flyspell-overlays-in
    	         (match-beginning 0) (match-end 0))
                (when (and (org-fold-core-folding-spec-p 'org-link)
                           (org-fold-core-folding-spec-p 'org-link-description))
                  (org-fold-region (match-beginning 0) (match-end 0) nil 'org-link)
                  (org-fold-region (match-beginning 0) (match-end 0) nil 'org-link-description))
    	        (remove-text-properties (match-beginning 2) (match-end 2)
    				                    '(display t invisible t intangible t)))
              (add-text-properties (match-beginning 2) (match-end 2)
    			                   '(font-lock-multiline t org-emphasis t))
              (when (and org-hide-emphasis-markers
    		             (not (org-at-comment-p)))
    	        (add-text-properties (match-end 4) (match-beginning 5)
    			                     '(invisible t))
    	        (add-text-properties (match-beginning 3) (match-end 3)
    			                     '(invisible t)))
              (throw :exit t))))))))

;;;###autoload
(defun eli/org-expand-all ()
  (interactive)
  (org-fold-show-subtree)
  (org-unlogged-message "ALL")
  (setq org-cycle-subtree-status 'all))

;; a TODO entry automatically change to DONE when all children are done
(defun eli/org-summary-todo (_n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;;; org-mode 9.6 workaround
(defun eli/org-return-wrapper (orig &rest args)
  "Wrap `org-return'."
  (if (and (or (not (boundp 'visible-mode)) (not visible-mode))
           (or (org-invisible-p)
    	       (org-invisible-p (max (point-min) (1- (point))))))
      (if (= (org-current-line)
             (org-current-line (point-max)))
          (insert "\n")
        (forward-line)
        (save-excursion
          (org-newline-and-indent)))
    (apply orig args)))

;; override the original version
;; src: https://stackoverflow.com/questions/17478260/completely-hide-the-properties-drawer-in-org-mode
(defun elemacs/org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change."
  (when (and (derived-mode-p 'org-mode)
             (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp
    		          (point-min)
                    (point)))
             (end (if globalp
    		          (point-max)
                    (if (eq state 'children)
    		            (save-excursion
                          (outline-next-heading)
                          (point))
    		          (org-end-of-subtree t)))))
        (goto-char beg)
        (while (re-search-forward org-drawer-regexp end t)
          (save-excursion
            (beginning-of-line 1)
            (when (looking-at org-drawer-regexp)
    	      (let* ((start (1- (match-beginning 0)))
    	             (limit
    		          (save-excursion
                        (outline-next-heading)
                        (point)))
    	             (msg (format
                           (concat
                            "org-cycle-hide-drawers:  "
                            "`:END:`"
                            " line missing at position %s")
                           (1+ start))))
                (if (re-search-forward "^[ \t]*:END:" limit t)
                    (outline-flag-region start (line-end-position) t)
                  (user-error msg))))))))))

(setq org-emphasis-regexp-components '("-[:space:]('\"{[:nonascii:]"
                                       "-[:space:].,:!?;'\")}\\[[:nonascii:]"
                                       "[:space:]"
                                       "."
                                       1))
(setq org-match-substring-regexp
	  (concat
	   ;; 限制上标和下标的匹配范围，org 中对其的介绍见：(org) Subscripts and superscripts
	   "\\([0-9a-zA-Zα-γΑ-Ω]\\)\\([_^]\\)\\("
	   "\\(?:" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
	   "\\|"
	   "\\(?:" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
	   "\\|"
	   "\\(?:\\*\\|[+-]?[[:alnum:].,\\]*[[:alnum:]]\\)\\)"))
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
(org-element-update-syntax)

(defun eli/clock-in-to-nest (_kw)
  (if (org-get-todo-state)
	  "STARTED"))


;; remove superfluous whitespace.
(defun eli/org2plaintxt (string)
  (cl-flet ((drop-markup (_ content _) (identity content)))
    (cl-letf (((symbol-function #'org-ascii-bold) #'drop-markup)
              ((symbol-function #'org-ascii-italic) #'drop-markup)
              ((symbol-function #'org-ascii-strike-through) #'drop-markup)
              ((symbol-function #'org-ascii-underline) #'drop-markup))
      (let ((org-ascii-text-width most-positive-fixnum)
			(org-ascii-bullets nil)
            (org-ascii-underline nil)
            (org-ascii-verbatim-format "%s"))
        (org-export-string-as string 'ascii t)))))

(defun eli/unfill-string (string)
  (if current-prefix-arg
	  (thread-last
		string
		eli/org2plaintxt
		;; (replace-regexp-in-string "\\([A-Za-z0-9]\\)\n" "\\1 ")
		;; (replace-regexp-in-string "\n" "" )
		)
    string))

;; movie rating

;;;###autoload
(defun eli/get-tag-counts ()
  (interactive)
  (let ((all-tags '()))
    (org-map-entries
     (lambda ()
	   (let ((tag-string (car (last (org-heading-components)))))
	     (when tag-string
	       (setq all-tags
		         (append all-tags (split-string tag-string ":" t))))))
     "+LEVEL=1")
    (list (completing-read "Select a tag:" all-tags))))

;;;###autoload
(defun eli/entry-rating ()
  (interactive)
  (let* ((eli/temp)
	     (eli/rate))
    (setq eli/temp (org-map-entries (lambda ()
                                      (string-to-number
                                       (if (org-entry-get nil "Rating")
                                           (org-entry-get nil "Rating")
                                         "0")))
                                    "+Rating>=0" `tree))
    (pop eli/temp)
    (setq eli/rate (if (= (length eli/temp) 0)
                       0
                     (/ (apply `+  eli/temp) (length eli/temp))))
    (org-set-property "Rating" (format "%.2f" eli/rate))))

;;;###autoload
(defun eli/rating (type)
  (interactive (eli/get-tag-counts))
  (org-map-entries 'eli/entry-rating
                   (concat type "+LEVEL=2-TODO=\"DONE\"-TODO=\"CANCELLED\"")))


;; rating films

;;;###autoload
(defun eli/get-film-rating ()
  (interactive)
  (let ((ratings)
	    (dimensions
         (list "剧情" "演技" "美术" "声效" "画面"
               "剪辑" "运镜" "立意" "人物" "细节")))
    (cl-loop for dim in dimensions
	         do
	         (push (string-to-number (org-entry-get (point) dim)) ratings))
    (org-entry-put (point) "Rating" (format "%.2f" (/ (-sum ratings) 10.0)))))

;;;###autoload
(defun eli/set-film-ratings ()
  (interactive)
  (let ((dimensions (list "剧情" "演技" "美术" "声效" "画面"
                          "剪辑" "运镜" "立意" "人物" "细节")))
    (cl-loop for dim in dimensions
	         do
	         (org-entry-put (point)
                            dim
                            (read-from-minibuffer
                             (format "Set rating for %s : " dim))))))


;; Learn from: https://xenodium.com/emacs-dwim-do-what-i-mean/

;;;###autoload
(defun ar/org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (and (not (null kill-ring))
                                   (string-match-p "^http" (current-kill 0)))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-link-make-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (org-link-make-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer
                                     (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag
                                               (libxml-parse-html-region
                                                (point-min)
                                                (point-max))
                                               'title))))))))
          (t
           (call-interactively 'org-insert-link)))))

;; show `#+name:' while in latex preview.
(defun eli/org-preview-show-label (orig-fun beg end &rest args)
  (let* ((beg (save-excursion
                (goto-char beg)
                (if (re-search-forward "#\\+name:" end t)
                    (progn
                      (forward-line)
                      (line-beginning-position))
                  beg))))
    (apply orig-fun beg end args)))


;;; adjust image size while adjusting the font size
(defvar eli/org-image-scale-mode-step 1.2
  "Scale factor used by `eli/org-image-scale-increase'.")

(defun eli/org-image-scale-increase (&rest _inc)
  (when org-inline-image-overlays
	(dolist (ov org-inline-image-overlays)
	  (image--set-property (overlay-get ov 'display)
						   :scale
						   (expt eli/org-image-scale-mode-step
								 text-scale-mode-amount)))))

;;;###autoload
(defun eli/org-narrow-to-item ()
  "Narrow buffer to the current item.

Throw an error when not in a list."
  (interactive)
  (save-excursion
    (narrow-to-region
	 (progn (org-beginning-of-item) (point))
	 (progn (org-end-of-item) (1- (point))))))

(defun eli/org-add-log-note (orig &rest args)
  (let* ((marker (copy-marker org-clock-marker))
		 (buffer (marker-buffer marker))
		 (pos (marker-position marker)))
	(apply orig args)
	(with-current-buffer buffer
	  (org-with-point-at pos
		(let* ((hd (nth 4 (org-heading-components)))
			   (prompt (concat "# Insert note for stopped clock."
							   "\n# Task: " hd "\n"))
			   (log (read-string-from-buffer prompt "")))
		  (unless (string-empty-p log)
			(end-of-line)
			(insert (format "\n- %s" log))))))))

;; better list format
;; (defun org-list-struct-fix-bul (struct prevs)
;;   "Verify and correct bullets in STRUCT.
;; PREVS is the alist of previous items, as returned by
;; `org-list-prevs-alist'.

;; This function modifies STRUCT."
;;   (let ((case-fold-search nil)
;; 	    (fix-bul
;; 	     ;; Set bullet of ITEM in STRUCT, depending on the type of
;; 	     ;; first item of the list, the previous bullet and counter
;; 	     ;; if any.
;; 	     (lambda (item)
;; 	       (let* ((prev (org-list-get-prev-item item struct prevs))
;; 		          (prev-bul (and prev (org-list-get-bullet prev struct)))
;; 		          (counter (org-list-get-counter item struct))
;; 		          (bullet (org-list-get-bullet item struct))
;; 		          (alphap (and (not prev)
;; 			                   (org-list-use-alpha-bul-p item struct prevs))))
;; 	         (org-list-set-bullet
;; 	          item struct
;; 	          (org-list-bullet-string
;; 	           (cond
;; 		        ;; Alpha counter in alpha list: use counter.
;; 		        ((and prev counter
;; 		              (string-match "[a-zA-Z]" counter)
;; 		              (string-match "[a-zA-Z]" prev-bul))
;; 		         ;; Use cond to be sure `string-match' is used in
;; 		         ;; both cases.
;; 		         (let ((real-count
;; 			            (cond
;; 			             ((string-match "[a-z]" prev-bul) (downcase counter))
;; 			             ((string-match "[A-Z]" prev-bul) (upcase counter)))))
;; 		           (replace-match real-count nil nil prev-bul)))
;; 		        ;; Num counter in a num list: use counter.
;; 		        ((and prev counter
;; 		              (string-match "[0-9]+" counter)
;; 		              (string-match "[0-9]+" prev-bul))
;; 		         (replace-match counter nil nil prev-bul))
;; 		        ;; No counter: increase, if needed, previous bullet.
;; 		        (prev
;; 		         (org-list-inc-bullet-maybe (org-list-get-bullet prev struct)))
;; 		        ;; Alpha counter at first item: use counter.
;; 		        ((and counter (org-list-use-alpha-bul-p item struct prevs)
;; 		              (string-match "[A-Za-z]" counter)
;; 		              (string-match "[A-Za-z]" bullet))
;; 		         (let ((real-count
;; 			            (cond
;; 			             ((string-match "[a-z]" bullet) (downcase counter))
;; 			             ((string-match "[A-Z]" bullet) (upcase counter)))))
;; 		           (replace-match real-count nil nil bullet)))
;; 		        ;; Num counter at first item: use counter.
;; 		        ((and counter
;; 		              (string-match "[0-9]+" counter)
;; 		              (string-match "[0-9]+" bullet))
;; 		         (replace-match counter nil nil bullet))
;; 		        ;; First bullet is alpha uppercase: use "A".
;; 		        ((and alphap (string-match "[A-Z]" bullet))
;; 		         (replace-match "A" nil nil bullet))
;; 		        ;; First bullet is alpha lowercase: use "a".
;; 		        ((and alphap (string-match "[a-z]" bullet))
;; 		         (replace-match "a" nil nil bullet))
;; 		        ;; First bullet is num: use "1".
;; 		        ((string-match "\\([0-9]+\\|[A-Za-z]\\)" bullet)
;; 		         ;; (replace-match "1" nil nil bullet))
;;                  (replace-match (format (format "%%0%dd" (1+ (log (length struct) 10))) 1) nil nil bullet))
;; 		        ;; Not an ordered list: keep bullet.
;; 		        (t bullet))))))))
;;     (mapc fix-bul (mapcar #'car struct))))

;; (defun org-list-inc-bullet-maybe (bullet)
;;   "Increment BULLET if applicable."
;;   (let ((case-fold-search nil))
;;     (cond
;;      ;; Num bullet: increment it.
;;      ((string-match "[0-9]+" bullet)
;;       (replace-match
;;        ;; (number-to-string (1+ (string-to-number (match-string 0 bullet))))
;;        (format (format "%%0%dd" (length (match-string 0 bullet)))
;;                (1+ (string-to-number (match-string 0 bullet))))
;;        nil nil bullet))
;;      ;; Alpha bullet: increment it.
;;      ((string-match "[A-Za-z]" bullet)
;;       (replace-match
;;        (char-to-string (1+ (string-to-char (match-string 0 bullet))))
;;        nil nil bullet))
;;      ;; Unordered bullet: leave it.
;;      (t bullet))))


;;;; provide
(provide 'lib-org)
;;; lib-org.el ends here.
