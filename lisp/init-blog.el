;; init-blog.el --- Initialize blog configurations.	-*- lexical-binding: t -*-

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

(with-eval-after-load 'org
  ;; publishing
  (defun eli/push-to-gitpage (&optional UNUSE)
    (interactive)
    (shell-command "~/.emacs.d/private/shell.sh")
    (message "blogs deployed successfully!")
    )

  (setq org-html-head-include-default-style nil)
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-validation-link nil) ; 去掉validation显示
  (setq org-html-link-home "index.html"); 设置home超链接
  (setq org-html-link-up "index.html")
  (setq eli/blog-base-dir "~/Dropbox/org/blog")
  (setq eli/blog-publish-dir "~/Elilif.github.io")
  (setq org-html-postamble nil)
  (setq org-publish-project-alist
	`(("eli's blog"
           :base-directory ,eli/blog-base-dir
           :publishing-directory ,eli/blog-publish-dir
           :base-extension "org"
           :recursive nil
	       :htmlized-source t
           :publishing-function org-html-publish-to-html
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Eli's blog"
           :sitemap-sort-files anti-chronologically
           :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>"
           :with-creator nil
           :completion-function eli/push-to-gitpage
           )))

  (defun org-html--format-image (source attributes info)
    (format "<img src=\"data:image/%s;base64,%s\"%s />"
            (or (file-name-extension source) "")
            (base64-encode-string
             (with-temp-buffer
	           (insert-file-contents-literally source)
	           (buffer-string)))
            (file-name-nondirectory source)))
  
  ;; (setq org-hugo-base-dir "~/Documents/braindump/")
  
  ;; (setq easy-hugo-basedir "~/Documents/braindump/"
  ;;       easy-hugo-postdir "content/posts")
  )



(provide 'init-blog)
;;; init-blog.el ends here.
