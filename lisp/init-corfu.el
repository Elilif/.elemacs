;; init-corfu.el --- Initialize corfu configurations.	-*- lexical-binding: t -*-

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

(add-hook 'elemacs-first-input-hook 'global-corfu-mode)
(with-eval-after-load 'corfu
  (setq corfu-cycle       t
	    corfu-auto        t
	    corfu-separator ?\s
	    corfu-max-width 150
	    corfu-auto-prefix 3
        corfu-excluded-modes '(org-mode)
	    corfu-on-exact-match nil))

;; use `all-the-icons' to show icons
(with-eval-after-load 'corfu
  (require 'all-the-icons)
  
  (defvar kind-all-the-icons--cache nil
    "The cache of styled and padded label (text or icon).
An alist.")

  (defun kind-all-the-icons-reset-cache ()
    "Remove all cached icons from `kind-all-the-icons-mapping'."
    (interactive)
    (setq kind-all-the-icons--cache nil))

  (defun kind-all-the-icons--set-default-clear-cache (&rest args)
    (kind-all-the-icons-reset-cache)
    (apply #'set-default args))

  (defvar kind-all-the-icons--icons
    `((unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
      (text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
      (method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
      (function . ,(all-the-icons-material "functions" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
      (fun . ,(all-the-icons-material "functions" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
      (constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
      (ctor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
      (field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
      (variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
      (var . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
      (class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
      (interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
      (i/f . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
      (module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
      (mod . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
      (property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
      (prop . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
      (unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
      (value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
      (enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
      (keyword . ,(all-the-icons-faicon "filter" :height 0.8 :v-adjust -0.15))
      (k/w . ,(all-the-icons-material "filter" :height 0.8 :v-adjust -0.15))
      (snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
      (sn . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
      (color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
      (file . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
      (reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
      (ref . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
      (folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
      (dir . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
      (enum-member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
      (enummember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
      (member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
      (constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
      (const . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
      (struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
      (event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
      (operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
      (op . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
      (type-parameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
      (param . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
      (template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15))
      (t . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))))


  (defsubst kind-all-the-icons--metadata-get (metadata type-name)
    (or
     (plist-get completion-extra-properties (intern (format ":%s" type-name)))
     (cdr (assq (intern type-name) metadata))))

  (defun kind-all-the-icons-formatted (kind)
    "Format icon kind with all-the-icons"
    (or (alist-get kind kind-all-the-icons--cache)
        (let ((map (assq kind kind-all-the-icons--icons)))
          (let*  ((icon (if map 
                            (cdr map) 
                          (cdr (assq t kind-all-the-icons--icons))))
                  (half (/ (default-font-width) 2))
                  (pad (propertize " " 'display `(space :width (,half))))
                  (disp (concat pad icon pad)))
            (setf (alist-get kind kind-all-the-icons--cache) disp)
            disp))))

  (defun kind-all-the-icons-margin-formatter (metadata)
    "Return a margin-formatter function which produces kind icons.
METADATA is the completion metadata supplied by the caller (see
info node `(elisp)Programmed Completion').  To use, add this
function to the relevant margin-formatters list."
    (if-let ((kind-func (kind-all-the-icons--metadata-get metadata "company-kind")))
        (lambda (cand)
	      (if-let ((kind (funcall kind-func cand)))
	          (kind-all-the-icons-formatted kind)
	        (kind-all-the-icons-formatted t))))) ;; as a backup

  (add-to-list 'corfu-margin-formatters 
               #'kind-all-the-icons-margin-formatter))

;; cape
;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)

;; cape-yasnippet
(add-hook 'emacs-lisp-mode-hook
		  (lambda ()
            (setq-local completion-at-point-functions
			            (list (cape-super-capf
                               #'cape-yasnippet
					           #'elisp-completion-at-point)
                              t))))

(defun cape-yasnippet--lsp ()
  "Create a super capf to include snippets in LSP completion."
  (setq-local completion-at-point-functions
              (list (cape-super-capf
                     #'lsp-completion-at-point
                     #'cape-yasnippet)
                    t)))
(add-hook 'lsp-completion-mode-hook #'cape-yasnippet--lsp)

(provide 'init-corfu)
;;; init-corfu.el ends here.
