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
  (defun eli/push-to-gitpage (&optional _args)
    (interactive)
    (start-process-shell-command "*publish*" nil "~/.emacs.d/private/shell.sh")
    (message "blogs deployed successfully!"))

  (defun eli/org-publish-rss-sitemap (title list)
    "Generate a sitemap of posts that is exported as a RSS feed.
TITLE is the title of the RSS feed.  LIST is an internal
representation for the files to include.  PROJECT is the current
project."
    (concat
     "#+TITLE: " title "\n\n"
     (org-list-to-subtree list)))

  (defun eli/org-publish-rss-entry (entry _style project)
    "Format ENTRY for the posts RSS feed in PROJECT."
    (let* ((file (org-publish--expand-file-name entry project))
           (preview (eli/blog-get-preview file))
           (title (org-publish-find-title entry project))
           (root (org-publish-property :html-link-home project))
           (link (concat (file-name-sans-extension entry) ".html"))
           (pubdate (format-time-string (cdr org-time-stamp-formats)
                                        (org-publish-find-date entry project))))
      (format "%s
:properties:
:rss_permalink: %s
:pubdate: %s
:end:\n%s\n[[%s][Read More]]"
              title
              link
              pubdate
              preview
              (concat
               root
               link))))

  (defun eli/blog-get-preview (file)
    "Get the contents of preview block in FILE."
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((beg (re-search-forward "^#\\+begin_preview\n" nil t))
            (end (progn (re-search-forward "^#\\+end_preview$" nil t)
                        (match-beginning 0))))
        (if beg
            (buffer-substring beg end)
          ""))))

  (defun eli/org-publish-rss-feed (plist filename dir)
    "Publish PLIST to Rss when FILENAME is rss.org.
DIR is the location of the output."
    (if (equal "rss.org" (file-name-nondirectory filename))
        (org-rss-publish-to-rss plist filename dir)))
  
  (setq org-html-head-include-default-style nil)
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-validation-link nil) ; 去掉validation显示
  (setq org-html-link-home "index.html"); 设置home超链接
  (setq org-html-link-up "index.html")
  (setq eli/blog-base-dir "~/Dropbox/org/blog")
  (setq eli/blog-publish-dir "~/Elilif.github.io")
  (setq org-html-postamble nil)
  (setq org-html-home/up-format "<div id=\"org-div-home-and-up\">\n <a accesskey=\"h\" href=\"rss.xml\"> RSS </a>\n |\n <a accesskey=\"H\" href=\"%s\"> HOME </a>\n</div>")
  (setq org-publish-project-alist
	    `(("eli's blog"
           :base-directory ,eli/blog-base-dir
           :publishing-directory ,eli/blog-publish-dir
           :base-extension "org"
           :recursive nil
	       :htmlized-source t
           :publishing-function org-html-publish-to-html
           :auto-sitemap t
           :exclude "rss.org"
           :sitemap-filename "index.org"
           :sitemap-title "Eli's Blog"
           :sitemap-sort-files anti-chronologically
           :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>"
           :html-preamble "<div><hr class=\"Solid\"></div>"
           :html-postamble "<hr class=\"Solid\">
<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Create Date: %d</p>
<p class=\"date\">Last modified: %C</p>
<p>Creator: %c</p>

<p style=\"text-align:center;\">
  <a rel=\"license\" href=\"http://creativecommons.org/licenses/by/4.0/\">
    <img alt=\"知识共享许可协议\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by/4.0/88x31.png\"/>
  </a><br />
  本作品采用
  <a rel=\"license\" href=\"http://creativecommons.org/licenses/by/4.0/\">
    知识共享署名 4.0 国际许可协议
  </a>
  进行许可。
</p>"
           :with-creator nil 
           ;; :completion-function eli/push-to-gitpage
           )
          ("eli's blog rss"
           :publishing-directory ,eli/blog-publish-dir
           :base-directory ,eli/blog-base-dir
           :base-extension "org"
           :include ("rss.org")
           :exclude "index.org"
           :publishing-function eli/org-publish-rss-feed
           :rss-extension "xml"
           :html-link-home "https://elilif.github.io/"
           :html-link-use-abs-url t
           :html-link-org-files-as-html t
           :auto-sitemap t
           :sitemap-function eli/org-publish-rss-sitemap
           :sitemap-title "Eli's Blog"
           :sitemap-filename "rss.org"
           :sitemap-sort-files anti-chronologically
           :sitemap-format-entry eli/org-publish-rss-entry
           )))
  
  (defun eli/filter-org-html--format-image (orig source attributes info)
    "Use base64 string instead of url to display images.

This functions is a advice for `org-html--format-image',
arguments, SOURCE ATTRIBUTES and INFO are like the arguments with
the same names of ORIG."
    (let ((image-html (funcall orig source attributes info))
          (image-base64 (format "data:image/%s+xml;base64,%s\"%s"
                                (or (file-name-extension source) "")
                                (base64-encode-string
                                 (with-temp-buffer
	                               (insert-file-contents-literally
                                    (file-relative-name
                                     (substring source 7)
                                     default-directory))
	                               (buffer-string)))
                                (file-name-nondirectory source))))
      (replace-regexp-in-string "img src=\"\\(.*?\\)\"" image-base64 image-html
                                nil nil 1)))
  (advice-add 'org-html--format-image
              :around #'eli/filter-org-html--format-image))


(provide 'init-blog)
;;; init-blog.el ends here.
