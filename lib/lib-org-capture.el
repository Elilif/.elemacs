;; lib-org-capture.el --- Initialize lib-org-capture configurations.    -*- lexical-binding: t; -*-

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
  (require 'org-crypt))

(defun eli/capture-report-date-file ()
  (let ((name (read-string "Name: ")))
    (expand-file-name (format "%s-%s.org"
                              (format-time-string "%Y-%m-%d")
                              name) "~/Dropbox/org/blog")))

;; add a property to create id
(defun eli/org-capture-maybe-create-id ()
  (when (org-capture-get :create-id)
    (org-id-get-create)))

;; from: https://stackoverflow.com/questions/21073859/is-there-a-way-
;; with-org-capture-templates-to-not-insert-a-line-if-initial-conten
(defun v-i-or-nothing ()
  (with-current-buffer (org-capture-get :original-buffer)
    (let ((v-i (plist-get org-store-link-plist :initial))
          (org-src-mode (replace-regexp-in-string
                         "-mode"
                         ""
                         (format "%s" major-mode)))
          (type (if (derived-mode-p 'prog-mode) "src" "quote")))
      (if (equal v-i "")
          ""
        (if (string= type "src")
            (concat (format "\n#+begin_%s %s\n" type org-src-mode)
                    v-i
                    (format "\n#+end_%s\n" type))
          (concat (format "\n#+begin_%s\n" type)
                  v-i
                  (format "\n#+end_%s\n" type)))))))

(defun v-a-or-nothing ()
  (with-current-buffer (org-capture-get :original-buffer)
    (let* ((v-a (plist-get org-store-link-plist :annotation))
           (v-a-empty-p (equal v-a ""))
           (file-name (buffer-file-name (current-buffer))))
      (cond
       (v-a-empty-p "")
       ((and (not v-a-empty-p) (eq major-mode 'org-mode))
        (concat "- reference :: "
                (replace-regexp-in-string "::\\(.*?\\)\\]\\[\\(.*?\\)\\]" "\\2" v-a nil nil 1)))
       ((and (not (string-match-p "::\\(.*?\\)\\]\\[\\(.*?\\)\\]" v-a))
             file-name)
        (concat "- reference :: "
                (substring v-a 0 -1)
                "["
                (file-name-nondirectory file-name)
                "]]"))
       (t
        (concat "- reference :: " v-a))))))

(defun v-i-or-nothing-word ()
  (let* ((v-i (plist-get org-store-link-plist :initial))
         (sentence (with-current-buffer (org-capture-get :original-buffer)
                     (thing-at-point 'sentence 'no-properties)))
         (new-string (string-clean-whitespace
                      (replace-regexp-in-string "\n" " " (if (string-empty-p v-i)
                                                             sentence
                                                           v-i)))))
    new-string))

;; better fill region in capture
(defun eli/fill-quote-and-checklist ()
  "Fill long quotes or checklist after capture them."
  (save-excursion
    (push-mark)
    (goto-char (point-min))
    (cond
     ((save-excursion
        (re-search-forward "#\\+begin_quote\n\\(\\(?:.*\n\\)*?\\)#\\+end_quote"
                           nil t))
      (fill-region (match-beginning 1)
                   (match-end 1)))
     ((save-excursion
        (re-search-forward "^- \\[ \\].*" nil t))
      (fill-region (match-beginning 0)
                   (match-end 0))))))

(defun eli/org-capture-fill-template (template)
  (string-trim (org-capture-fill-template (if (symbolp template)
                                              (symbol-value template)
                                            template))))

;; stolen from org-roam
(defun eli/org-capture-find-or-create-olp (path)
  "Return a marker pointing to the entry at OLP in the current buffer.
If OLP does not exist, create it. If anything goes wrong, throw
an error, and if you need to do something based on this error,
you can catch it with `condition-case'."
  (let* ((file (pop path))
         (buffer (find-file-noselect file)))
    (unless buffer (error "File not found :%s" file))
    (with-current-buffer buffer
      (let* ((level 1)
             (lmin 1)
             (lmax 1)
             (start (point-min))
             (end (point-max))
             found flevel)
        (unless (derived-mode-p 'org-mode)
          (error "Buffer %s needs to be in Org mode" (current-buffer)))
        (org-with-wide-buffer
         (goto-char start)
         (dolist (heading path)
           (setq heading (eli/org-capture-fill-template heading))
           (let ((re (format org-complex-heading-regexp-format
                             (regexp-quote heading)))
                 (cnt 0))
             (while (re-search-forward re end t)
               (setq level (- (match-end 1) (match-beginning 1)))
               (when (and (>= level lmin) (<= level lmax))
                 (setq found (match-beginning 0) flevel level cnt (1+ cnt))))
             (when (> cnt 1)
               (error "Heading not unique on level %d: %s" lmax heading))
             (when (= cnt 0)
               ;; Create heading if it doesn't exist
               (goto-char end)
               (unless (bolp) (newline))
               (let (org-insert-heading-respect-content)
                 (org-insert-heading nil nil t))
               (unless (= lmax 1)
                 (dotimes (_ level) (org-do-demote)))
               (insert heading)
               (setq end (point))
               (goto-char start)
               (while (re-search-forward re end t)
                 (setq level (- (match-end 1) (match-beginning 1)))
                 (when (and (>= level lmin) (<= level lmax))
                   (setq found (match-beginning 0) flevel level cnt (1+ cnt))))))
           (goto-char found)
           (setq lmin (1+ flevel) lmax (+ lmin (if org-odd-levels-only 1 0)))
           (setq start found
                 end (save-excursion (org-end-of-subtree t t))))
         (point-marker))))))

(defun eli/org-capture-set-target-location (&optional target)
  "Find TARGET buffer and position.
Store them in the capture property list."
  (let ((target-entry-p t))
    (save-excursion
      (pcase (or target (org-capture-get :target))
        (`here
         (org-capture-put :exact-position (point) :insert-here t))
        (`(file ,path)
         (when (org-capture-get :if-new)
           (setq path (eli/org-capture-fill-template path)))
         (set-buffer (org-capture-target-buffer path))
         (org-capture-put-target-region-and-position)
         (widen)
         (setq target-entry-p nil))
        (`(id ,id)
         (pcase (org-id-find id)
           (`(,path . ,position)
            (set-buffer (org-capture-target-buffer path))
            (widen)
            (org-capture-put-target-region-and-position)
            (goto-char position))
           (_ (error "Cannot find target ID \"%s\"" id))))
        (`(file+headline ,path ,headline)
         (when (org-capture-get :if-new)
           (setq path (eli/org-capture-fill-template path)
                 headline (eli/org-capture-fill-template headline)))
         (set-buffer (org-capture-target-buffer path))
         ;; Org expects the target file to be in Org mode, otherwise
         ;; it throws an error.  However, the default notes files
         ;; should work out of the box.  In this case, we switch it to
         ;; Org mode.
         (unless (derived-mode-p 'org-mode)
           (org-display-warning
            (format "Capture requirement: switching buffer %S to Org mode"
                    (current-buffer)))
           (org-mode))
         (org-capture-put-target-region-and-position)
         (widen)
         (goto-char (point-min))
         (if (re-search-forward (format org-complex-heading-regexp-format
                                        (regexp-quote headline))
                                nil t)
             (beginning-of-line)
           (if (org-capture-get :prepend)
               (progn
                 (goto-char (point-min))
                 (unless (org-at-heading-p)
                   (org-next-visible-heading 1))
                 (open-line 1))
             (goto-char (point-max))
             (unless (bolp) (insert "\n")))
           (insert "* " headline "\n")
           (beginning-of-line 0)))
        (`(file+olp ,path . ,outline-path)
         (when (org-capture-get :if-new)
           (setq path (eli/org-capture-fill-template path)))
         (let ((m (eli/org-capture-find-or-create-olp
                   (cons (org-capture-expand-file path)
                         outline-path))))
           (set-buffer (marker-buffer m))
           (org-capture-put-target-region-and-position)
           (widen)
           (goto-char m)
           (set-marker m nil)))
        (`(file+regexp ,path ,regexp)
         (when (org-capture-get :if-new)
           (setq path (eli/org-capture-fill-template path)))
         (set-buffer (org-capture-target-buffer path))
         (org-capture-put-target-region-and-position)
         (widen)
         (goto-char (point-min))
         (if (not (re-search-forward regexp nil t))
             (error "No match for target regexp in file %s" path)
           (goto-char (if (org-capture-get :prepend)
                          (match-beginning 0)
                        (match-end 0)))
           (org-capture-put :exact-position (point))
           (setq target-entry-p
                 (and (derived-mode-p 'org-mode) (org-at-heading-p)))))
        (`(file+olp+datetree ,path . ,outline-path)
         (when (org-capture-get :if-new)
           (setq path (eli/org-capture-fill-template path)))
         (let ((m (if outline-path
                      (eli/org-capture-find-or-create-olp
                       (cons (org-capture-expand-file path)
                             outline-path))
                    (set-buffer (org-capture-target-buffer path))
                    (point-marker))))
           (set-buffer (marker-buffer m))
           (org-capture-put-target-region-and-position)
           (widen)
           (goto-char m)
           (set-marker m nil)
           (require 'org-datetree)
           (org-capture-put-target-region-and-position)
           (widen)
           ;; Make a date/week tree entry, with the current date (or
           ;; yesterday, if we are extending dates for a couple of
           ;; hours)
           (funcall
            (pcase (org-capture-get :tree-type)
              (`week #'org-datetree-find-iso-week-create)
              (`month #'org-datetree-find-month-create)
              (_ #'org-datetree-find-date-create))
            (calendar-gregorian-from-absolute
             (cond
              (org-overriding-default-time
               ;; Use the overriding default time.
               (time-to-days org-overriding-default-time))
              ((or (org-capture-get :time-prompt)
                   (equal current-prefix-arg 1))
               ;; Prompt for date.  Bind `org-end-time-was-given' so
               ;; that `org-read-date-analyze' handles the time range
               ;; case and returns `prompt-time' with the start value.
               (let* ((org-time-was-given nil)
                      (org-end-time-was-given nil)
                      (prompt-time (org-read-date
                                    nil t nil "Date for tree entry:")))
                 (org-capture-put
                  :default-time
                  (if (or org-time-was-given
                          (= (time-to-days prompt-time) (org-today)))
                      prompt-time
                    ;; Use 00:00 when no time is given for another
                    ;; date than today?
                    (org-encode-time
                     (apply #'list
                            0 0 org-extend-today-until
                            (cl-cdddr (decode-time prompt-time))))))
                 (time-to-days prompt-time)))
              (t
               ;; Current date, possibly corrected for late night
               ;; workers.
               (org-today))))
            ;; the following is the keep-restriction argument for
            ;; org-datetree-find-date-create
            (when outline-path 'subtree-at-point))))
        (`(file+function ,path ,function)
         (when (org-capture-get :if-new)
           (setq path (eli/org-capture-fill-template path)))
         (set-buffer (org-capture-target-buffer path))
         (org-capture-put-target-region-and-position)
         (widen)
         (funcall function)
         (org-capture-put :exact-position (point))
         (setq target-entry-p
               (and (derived-mode-p 'org-mode) (org-at-heading-p))))
        (`(function ,fun)
         (funcall fun)
         (org-capture-put :exact-position (point))
         (setq target-entry-p
               (and (derived-mode-p 'org-mode) (org-at-heading-p))))
        (`(clock)
         (if (and (markerp org-clock-hd-marker)
                  (marker-buffer org-clock-hd-marker))
             (progn (set-buffer (marker-buffer org-clock-hd-marker))
                    (org-capture-put-target-region-and-position)
                    (widen)
                    (goto-char org-clock-hd-marker))
           (user-error "No running clock that could be used as capture target")))
        (target (error "Invalid capture target specification: %S" target)))

      (org-capture-put :buffer (current-buffer)
                       :pos (point)
                       :target-entry-p target-entry-p
                       :decrypted
                       (and (featurep 'org-crypt)
                            (org-at-encrypted-entry-p)
                            (save-excursion
                              (org-decrypt-entry)
                              (and (org-back-to-heading t) (point))))))))


;;;; provide
(provide 'lib-org-capture)
;;; lib-org-capture.el ends here.
