;; lib-tempel.el --- Initialize lib-tempel configurations.  -*- lexical-binding: t; -*-

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

(defun tempel-include (elt)
  (when (eq (car-safe elt) 'i)
    (if-let (template (alist-get (cadr elt) (tempel--templates)))
        (cons 'l template)
      (message "Template %s not found" (cadr elt))
      nil)))

(add-to-list 'tempel-user-elements #'tempel-include)

(defun eli/abbrev--case-fixed (args)
  (when (length= args 3)
    (setq args (append args '(nil))))
  (plist-put args :case-fixed t)
  args)
(advice-add 'define-abbrev :filter-args #'eli/abbrev--case-fixed)

(defun eli/tempel-expand ()
  "Complete with CAPF."
  (let ((completion-at-point-functions (list #'tempel-expand))
        completion-cycle-threshold)
    (tempel--save)
    (completion-at-point)))

(defun tempel-setup-capf ()
  ;; Add the Tempel Capf to `completion-at-point-functions'.
  ;; `tempel-expand' only triggers on exact matches. Alternatively use
  ;; `tempel-complete' if you want to see all matches, but then you
  ;; should also configure `tempel-trigger-prefix', such that Tempel
  ;; does not trigger too often when you don't expect it. NOTE: We add
  ;; `tempel-expand' *before* the main programming mode Capf, such
  ;; that it will be tried first.
  (setq-local completion-at-point-functions
              (cons #'eli/tempel-expand
                    completion-at-point-functions)))

(defun eli/tempel--exit (templates region name status)
  "Exit function for completion for template NAME and STATUS.
TEMPLATES is the list of templates.
REGION are the current region bounds."
  (unless (eq status 'exact)
    (when-let ((sym (intern-soft name))
               (template (alist-get sym templates))
               (plist template))
      (while (and plist (not (keywordp (car plist))))
        (pop plist))
      (when (or (eval (plist-get plist :auto) 'lexical)
                (eq this-command 'smarter-tab-to-expand)
                (eq this-command 'tempel-expand))
        (tempel--delete-word name)
        (when tempel-trigger-prefix
          (tempel--delete-word tempel-trigger-prefix))
        (tempel--insert template region)))))

(defun eli/tempel--get-prefix-bounds ()
  (let* ((beg (save-excursion
                (re-search-backward "\\s-"
                                    (line-beginning-position) 'noerror)))
         (beg (if beg (1+ beg) (line-beginning-position))))
    (cons beg (point))))

(defun tempel--prefix-bounds ()
  "Return prefix bounds."
  (if tempel-trigger-prefix
      (let ((end (point))
            (beg (save-excursion
                   (search-backward tempel-trigger-prefix
                                    (line-beginning-position) 'noerror))))
        (when (and beg (save-excursion
                         (not (re-search-backward "\\s-" beg 'noerror))))
          (cons (+ beg (length tempel-trigger-prefix)) end)))
    (eli/tempel--get-prefix-bounds)))



;;; SRC: https://emacs-china.org/t/topic/21079/32?u=vagrantjoker
(defvar smarter-tab-to-expand-in-use nil)
(defvar eli/smarter-tab-commands '(tempel-expand org-cycle tempel-next))

(defun smarter-tab-to-expand ()
  "Try to `org-cycle', `tempel-expand' at current cursor position.
If all failed, try to complete the common part with `indent-for-tab-command'."
  (interactive)
  (if (and (not smarter-tab-to-expand-in-use)
           (featurep 'tempel))
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick))
            (func-list
             (if (equal major-mode 'org-mode)
                 eli/smarter-tab-commands
               '(tempel-expand tempel-next))))
        (catch 'func-suceed
          (setq smarter-tab-to-expand-in-use t)
          (dolist (func func-list)
            (ignore-errors (call-interactively func))
            (unless (and (eq old-point (point))
                         (eq old-tick (buffer-chars-modified-tick)))
              (setq smarter-tab-to-expand-in-use nil)
              (throw 'func-suceed t)))
          (setq smarter-tab-to-expand-in-use nil)
          (indent-for-tab-command)))
    (indent-for-tab-command)))

(defun eli/tempel-get-tex-object ()
  "Get tex object before point."
  (save-excursion
    (pcase (progn
             (skip-chars-backward " \t" (line-beginning-position))
             (char-before))
      ((or ?\) ?\])
       (backward-sexp)
       (point))
      (?\}
       (cl-loop do (backward-sexp)
                while (= (char-before) ?\}))
       (or (save-excursion
             (re-search-backward "\\\\" (line-beginning-position) 'noerror))
           (point)))
      ((pred (lambda (char)
               (not (memq char '(?\C-j)))))
       (backward-word)
       (point)))))

(defun eli/tempel-tex-smart-kill ()
  (when-let* ((beg (eli/tempel-get-tex-object))
              (end (point)))
    (kill-region beg end)))

(defun eli/tempel-tex-smart-paste ()
  "Paste text killed by `eli/latex-smart-kill'."
  (let ((temp (string-clean-whitespace (current-kill 0))))
    (if (string-match "^(\\(.*\\))$" temp)
        (match-string 1 temp)
      temp)))

;; C/C++ mode
;; (defun eli/c-fun-has-namespace-p (namespace)
;;   "Predicate whether the current function has NAMESPACE namespace."
;;   (save-excursion
;;     (c-beginning-of-defun)
;;     (unless (re-search-forward
;;              (concat "using namespace "
;;                      namespace
;;                      ";")
;;              (save-excursion
;;                (c-end-of-defun)
;;                (point)) 'no-errer)
;;       (concat namespace "::"))))

(defvar eli/tempel-temp-templates nil)

(defun eli/tempel-temp-parse (str)
  "Parse STR as a tempel template."
  (let ((start 0)
        (regex (format
                "%s\\(?1:\\(?:%s\\)>?\\|\\(?:%s\\)\\|\\(?:([^%s]*\\)\\)"
                "~"
                "\\sw"
                ">"
                "~"))
        res
        obj)
    (while (string-match regex str start)
      (when-let ((text (substring str start (match-beginning 0)))
                 ((not (string-empty-p text))))
        (push text res))
      (setq obj (read (match-string 1 str)))
      (push obj res)
      (setq start (- (match-end 0)
                     (- (length (match-string 1 str))
                        (length (prin1-to-string obj))))))
    (when-let ((text (substring str start))
               ((not (string-empty-p text))))
      (push text res))
    (nreverse res)))

(defun eli/tempel-temp-create (beg end)
  "Create a temporary tempel template."
  (interactive "r")
  (let* ((template (eli/tempel-temp-parse
                    (thread-last
                      (buffer-substring-no-properties beg end)
                      (replace-regexp-in-string
                       "\\(\n[)}]\\)" "\\1~>")
                      (replace-regexp-in-string
                       "\n +" "~n>")
                      (replace-regexp-in-string
                       "\n" "~n"))))
         (name (read-minibuffer "Create template name: "))
         (existp (assq name (alist-get major-mode eli/tempel-temp-templates))))
    (if (and (assq name (tempel--templates))
             (not existp))
        (user-error "%s already exists!" (symbol-name name))
      (when (or (not existp)
                (and existp
                     (y-or-n-p (format "%s already exists, override it?"
                                       (symbol-name name)))))
        (setf (alist-get name (alist-get major-mode eli/tempel-temp-templates))
              template)
        (delete-region beg end)
        (tempel-insert name)))
    (when current-prefix-arg
      (eli/tempel-temp-save name))))

(defun eli/tempel-temp-save (name)
  "Save the selected temporary template."
  (interactive (list
                (intern (completing-read
                         "Select: "
                         (alist-get major-mode eli/tempel-temp-templates)))))
  (let* ((templates (alist-get major-mode eli/tempel-temp-templates))
         (template (prin1-to-string (assoc name templates)))
         (major (symbol-name major-mode)))
    (with-current-buffer (find-file-noselect tempel-path)
      (goto-char (point-min))
      (if (search-forward major (point-max) t)
          (progn
            (re-search-forward "^(")
            (backward-char)
            (insert template)
            (insert "\n"))
        (search-forward ";; Local Variables:" (point-max) t)
        (goto-char (match-beginning 0))
        (insert major)
        (insert "\n\n")
        (insert template)
        (insert "\n\n"))
      (save-buffer))))

(defun eli/tempel-temp-templates ()
  (alist-get major-mode eli/tempel-temp-templates))

;; (defun eli/tempel--placeholder (st &optional prompt name noinsert)
;;   (setq prompt
;;         (cond
;;          ((and (stringp prompt) noinsert) (read-string prompt))
;;          ((stringp prompt) (propertize prompt 'tempel--default t))
;;          ;; TEMPEL EXTENSION: Evaluate prompt
;;          (t (eval prompt (cdr st)))))
;;   (if noinsert
;;       (progn (setf (alist-get name (cdr st)) prompt) nil)
;;     (setq prompt (or prompt (alist-get name (cdr st))))
;;     (setf (alist-get name (cdr st)) prompt)
;;     (tempel--form st name)))

;; (defun eli/tempel--element (st region elt)
;;   "Add template ELT to ST given the REGION."
;;   (pcase elt
;;     ('nil)
;;     ('n (insert "\n"))
;;     ;; `indent-according-to-mode' fails sometimes in Org. Ignore errors.
;;     ('n> (insert "\n") (tempel--protect (indent-according-to-mode)))
;;     ('> (tempel--protect (indent-according-to-mode)))
;;     ((pred stringp) (insert elt))
;;     ('& (unless (or (bolp) (save-excursion (re-search-backward "^\\s-*\\=" nil t)))
;;           (insert "\n")))
;;     ('% (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
;;           (insert "\n")))
;;     ('o (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
;;           (open-line 1)))
;;     (`(s ,name) (tempel--field st name))
;;     (`(l . ,lst) (dolist (e lst) (tempel--element st region e)))
;;     ((or 'p `(,(or 'p 'P) . ,rest)) (apply #'tempel--placeholder st rest))
;;     ((or 'r 'r> `(,(or 'r 'r>) . ,rest))
;;      (if (not region)
;;          (when-let ((ov (apply #'tempel--placeholder st rest))
;;                     ((not rest)))
;;            (overlay-put ov 'tempel--enter #'tempel--done))
;;        (goto-char (cdr region))
;;        (when (eq (or (car-safe elt) elt) 'r>)
;;          (indent-region (car region) (cdr region) nil))))
;;     ;; TEMPEL EXTENSION: Quit template immediately
;;     ('q (overlay-put (tempel--field st) 'tempel--enter #'tempel--done))
;;     (`(f . ,rest) (apply #'eli/tempel--placeholder st rest))
;;     (_ (if-let (ret (run-hook-with-args-until-success 'tempel-user-elements elt))
;;            (tempel--element st region ret)
;;          ;; TEMPEL EXTENSION: Evaluate forms
;;          (tempel--form st elt)))))

;;;; provide
(provide 'lib-tempel)
;;; lib-tempel.el ends here.
