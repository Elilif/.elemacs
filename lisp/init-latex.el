;; init-latex.el --- Initialize latex configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 by Eli

;; Author: Eli <eli.q.qian@gmail.com>
;; URL: https://github.com/Elilif/.emacs.d

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

(with-eval-after-load 'tex
  (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook #'tex-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-command-default "XeLaTeX")
              (add-hook 'TeX-update-style-hook
                        (lambda ()
                          (when (member "biblatex" TeX-active-styles)
                            (setq-local LaTeX-biblatex-use-Biber t)))
                        -100
                        t)))
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (setq-default TeX-engine 'xetex)
  (setq LaTeX-command "xelatex")
  (setq TeX-source-correlate-start-server t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq tex-fontify-script nil)
  (setq font-latex-fontify-script nil)
  (setq LaTeX-item-indent 0)
  (setq TeX-show-compilation nil)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --shell-escape --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
  
  ;; reftex
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography eli/bibliography)
  )

(with-eval-after-load 'tex
  (add-hook 'LaTeX-mode-hook #'lsp-deferred)
  (setq lsp-latex-texlab-executable "/usr/bin/texlab")
  (setq lsp-tex-server 'texlab))

(with-eval-after-load 'tex
  (require 'cdlatex)
  (keymap-set cdlatex-mode-map "$" nil)
  (keymap-set cdlatex-mode-map "\(" nil)
  (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
  (add-hook 'cdlatex-tab-hook #'cdlatex-in-yas-field)

  (defun cdlatex-in-yas-field ()
    ;; Check if we're at the end of the Yas field
    (when-let* ((_ (overlayp yas--active-field-overlay))
                (end (overlay-end yas--active-field-overlay)))
      (if (>= (point) end)
          ;; Call yas-next-field if cdlatex can't expand here
          (let ((s (thing-at-point 'sexp)))
            (unless (and s (assoc (substring-no-properties s)
                                  cdlatex-command-alist-comb))
              (yas-next-field-or-maybe-expand)
              t))
        ;; otherwise expand and jump to the correct location
        (let (cdlatex-tab-hook minp)
          (setq minp
                (min (save-excursion (cdlatex-tab)
                                     (point))
                     (overlay-end yas--active-field-overlay)))
          (goto-char minp) t))))

  (defun yas-next-field-or-cdlatex ()
    "Jump to the next Yas field correctly with cdlatex active."
    (interactive)
    (if (bound-and-true-p cdlatex-mode)
        (cdlatex-tab)
      (yas-next-field-or-maybe-expand)))
  (setq cdlatex-paired-parens "$[{")
  )
;; yasnippet support
(with-eval-after-load 'tex
  (add-hook 'cdlatex-tab-hook #'yas-expand)
  (keymap-set cdlatex-mode-map "<tab>" #'cdlatex-tab)
  (keymap-set yas-keymap "<tab>" #'yas-next-field-or-cdlatex)
  (keymap-set yas-keymap "TAB" #'yas-next-field-or-cdlatex))

(add-hook 'org-mode-hook
          (lambda ()
            (unless (member (buffer-file-name)
                            (append '("/home/eli/Dropbox/org/Clock_Report.org")
                                    org-agenda-files
                                    (mapcar (lambda (x)
                                              (concat x "_archive"))
                                            org-agenda-files)))
              (xenops-mode)))) ;; preview latex fragments in org-mode
(add-hook 'LaTeX-mode-hook #'xenops-mode)

(with-eval-after-load 'xenops
  (setq xenops-math-image-scale-factor 1.3
        xenops-image-try-write-clipboard-image-to-file nil
        xenops-reveal-on-entry nil
        xenops-math-image-margin 0
        xenops-math-latex-max-tasks-in-flight 16)
  (defun eli/change-xenops-latex-header (orig &rest args)
    (let ((org-format-latex-header "\\documentclass[dvisvgm,preview]{standalone}\n\\usepackage{arev}\n\\usepackage{color}\n[PACKAGES]\n[DEFAULT-PACKAGES]"))
      (apply orig args)))
  (advice-add 'xenops-math-latex-make-latex-document :around #'eli/change-xenops-latex-header)
  (advice-add 'xenops-math-file-name-static-hash-data :around #'eli/change-xenops-latex-header)
  
  (defun eli/delete-region ()
    (if (use-region-p)
        (delete-region (region-beginning)
                       (region-end))))
  (advice-add 'xenops-handle-paste-default
              :before #'eli/delete-region)

  (defun eli/xenops-math-add-cursor-sensor-property ()
    (-when-let* ((element (xenops-math-parse-element-at-point)))
      (let ((beg (plist-get element :begin))
            (end (plist-get element :end))
            (props '(cursor-sensor-functions (xenops-math-handle-element-transgression))))
        (add-text-properties beg end props)
        (add-text-properties (1- end) end '(rear-nonsticky (cursor-sensor-functions))))))
  (advice-add 'xenops-math-add-cursor-sensor-property :override #'eli/xenops-math-add-cursor-sensor-property)
  
  ;; Vertically align LaTeX preview in org mode
  (setq xenops-math-latex-process-alist
        '((dvisvgm :programs
                   ("latex" "dvisvgm")
                   :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
                   (1.7 . 1.5)
                   :latex-compiler
                   ("latex -interaction nonstopmode -shell-escape -output-format dvi -output-directory %o %f")
                   :image-converter
                   ("dvisvgm %f -n -e -b 1 -c %S -o %O"))))
  
  ;; from https://list.orgmode.org/874k9oxy48.fsf@gmail.com/#Z32lisp:org.el
  (defun eli/org--match-text-baseline-ascent (imagefile)
    "Set `:ascent' to match the text baseline of an image to the surrounding text.
Compute `ascent' with the data collected in IMAGEFILE."
    (let* ((viewbox (split-string
                     (xml-get-attribute (car (xml-parse-file imagefile)) 'viewBox)))
           (min-y (string-to-number (nth 1 viewbox)))
           (height (string-to-number (nth 3 viewbox)))
           (ascent (round (* -100 (/ min-y height)))))
      (if (or (< ascent 0) (> ascent 100))
          'center
        ascent)))

  (defun eli/xenops-preview-align-baseline (element &rest _args)
    "Redisplay SVG image resulting from successful LaTeX compilation of ELEMENT.

Use the data in log file (e.g. \"! Preview: Snippet 1 ended.(368640+1505299x1347810).\")
to calculate the decent value of `:ascent'. "
    (let* ((inline-p (eq 'inline-math (plist-get element :type)))
           (ov-beg (plist-get element :begin))
           (ov-end (plist-get element :end))
           (cache-file (car (last _args)))
           (ov (car (overlays-at (/ (+ ov-beg ov-end) 2) t)))
           img new-img ascent)
      (when (and ov inline-p)
        (setq ascent (+ 1 (eli/org--match-text-baseline-ascent cache-file)))
        (setq img (cdr (overlay-get ov 'display)))
        (setq new-img (plist-put img :ascent ascent))
        (overlay-put ov 'display (cons 'image new-img)))))
    (advice-add 'xenops-math-display-image :after
                #'eli/xenops-preview-align-baseline)  

  ;; from: https://kitchingroup.cheme.cmu.edu/blog/2016/11/06/
  ;; Justifying-LaTeX-preview-fragments-in-org-mode/
  ;; specify the justification you want
  (plist-put org-format-latex-options :justify 'right)
  
  (defun eli/xenops-justify-fragment-overlay (element &rest _args)
    (let* ((ov-beg (plist-get element :begin))
           (ov-end (plist-get element :end))
           (ov (car (overlays-at (/ (+ ov-beg ov-end) 2) t)))
           (position (plist-get org-format-latex-options :justify))
           (inline-p (eq 'inline-math (plist-get element :type)))
           width offset)
      (when (and ov
                 (imagep (overlay-get ov 'display)))
        (setq width (car (image-display-size (overlay-get ov 'display))))
        (cond
         ((and (eq 'right position) 
               (not inline-p)
               (> width 50))
          (setq offset (floor (- fill-column
                                 width)))
          (if (< offset 0)
              (setq offset 0))
          (overlay-put ov 'before-string (make-string offset ? )))
         ((and (eq 'right position)
               (not inline-p))
          (setq offset (floor (- (/ fill-column 2)
                                 (/ width 2))))
          (if (< offset 0)
              (setq offset 0))
          (overlay-put ov 'before-string (make-string offset ? )))))))
  (advice-add 'xenops-math-display-image :after
              #'eli/xenops-justify-fragment-overlay)


  ;; from: https://kitchingroup.cheme.cmu.edu/blog/2016/11/07/
  ;; Better-equation-numbering-in-LaTeX-fragments-in-org-mode/  
  (defun eli/xenops-renumber-environment (orig-func element latex colors
                                                    cache-file display-image)
    (let ((results '()) 
          (counter -1)
          (numberp))
      (setq results (cl-loop for (begin .  env) in 
                             (org-element-map (org-element-parse-buffer)
                                 'latex-environment
                               (lambda (env)
                                 (cons
                                  (org-element-property :begin env)
                                  (org-element-property :value env))))
                             collect
                             (cond
                              ((and (string-match "\\\\begin{equation}" env)
                                    (not (string-match "\\\\tag{" env)))
                               (cl-incf counter)
                               (cons begin counter))
                              ((and (string-match "\\\\begin{align}" env)
                                    (string-match "\\\\notag" env))
                               (cl-incf counter)
                               (cons begin counter))
                              ((string-match "\\\\begin{align}" env)
                               (prog2
                                   (cl-incf counter)
                                   (cons begin counter)                          
                                 (with-temp-buffer
                                   (insert env)
                                   (goto-char (point-min))
                                   ;; \\ is used for a new line. Each one leads
                                   ;; to a number
                                   (cl-incf counter (count-matches "\\\\$"))
                                   ;; unless there are nonumbers.
                                   (goto-char (point-min))
                                   (cl-decf counter
                                            (count-matches "\\nonumber")))))
                              (t
                               (cons begin nil)))))
      (when (setq numberp (cdr (assoc (plist-get element :begin) results)))
        (setq latex
              (concat
               (format "\\setcounter{equation}{%s}\n" numberp)
               latex))))
    (funcall orig-func element latex colors cache-file display-image))
  (advice-add 'xenops-math-latex-create-image :around #'eli/xenops-renumber-environment))

(autoload #'mathpix-screenshot "mathpix" nil t)
(with-eval-after-load 'mathpix
  (let ((n (random 4)))
    (setq mathpix-app-id (with-temp-buffer
                           (insert-file-contents
                            "~/.emacs.d/private/mathpix-app-id")
                           (nth n (split-string (buffer-string) "\n")))
          mathpix-app-key (with-temp-buffer
                            (insert-file-contents
                             "~/.emacs.d/private/mathpix-app-key")
                            (nth n (split-string (buffer-string) "\n")))))
  (setq mathpix-screenshot-method "flameshot gui --raw > %s"))

(provide 'init-latex)
;;; init-latex.el ends here.
