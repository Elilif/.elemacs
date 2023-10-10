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

(setup ox-html
  (:option*
   org-html-head-include-default-style nil
   org-html-htmlize-output-type 'css
   org-html-validation-link nil
   org-html-link-home "https://elilif.github.io/index.html"
   org-html-link-up "https://elilif.github.io/rss.xml"
   org-html-home/up-format "<div id=\"org-div-home-and-up\">\n <a accesskey=\"h\" href=\"%s\"> RSS </a>\n |\n <a accesskey=\"H\" href=\"%s\"> HOME </a>\n</div>"
   org-html-postamble nil))

(setup ox-publish
  (:when-loaded
    (require 'lib-ox-publish))
  (:option*
   eli/blog-base-dir "~/Dropbox/org/blog"
   eli/blog-publish-dir "~/Elilif.github.io"
   eli/blog-sitamap "index.org"
   org-publish-project-alist
   `(("eli's blog"
      :base-directory ,eli/blog-base-dir
      :publishing-directory ,(expand-file-name "articles" eli/blog-publish-dir)
      :base-extension "org"
      :recursive nil
      :htmlized-source t
      :publishing-function org-html-publish-to-html
      :exclude "rss.org"

      :auto-sitemap t
      :sitemap-filename ,eli/blog-sitamap
      :sitemap-title "Eli's Blog"
      :sitemap-sort-files anti-chronologically
      :sitemap-function eli/org-publish-sitemap
      :sitemap-format-entry eli/sitemap-dated-entry-format
      :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/style.css\"/>"
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
      :completion-function eli/blog-publish-completion
      )
     ("eli's blog rss"
      :preparation-function eli/publish-rss-papare
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
      :sitemap-format-entry eli/org-publish-rss-entry)))
  (:after ox
    (add-to-list 'org-export-global-macros
                 '("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")))
  (:advice
   org-html--format-image :around eli/filter-org-html--format-image
   org-publish :before (lambda (&rest _args) (org-publish-reset-cache))))


(provide 'init-blog)
;;; init-blog.el ends here.
