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

(setup ox
  (:option*
   org-export-with-priority t
   org-export-with-toc 4
   org-export-with-section-numbers nil
   org-export-with-planning t
   org-export-with-special-strings nil))

(setup ox-html
  (:option*
   org-html-metadata-timestamp-format "%Y-%m-%d"
   org-html-head-include-default-style nil
   org-html-htmlize-output-type 'css
   org-html-validation-link nil
   org-html-prefer-user-labels t
   org-html-head-include-scripts t
   org-html-wrap-src-lines nil
   org-html-checkbox-type 'html
   org-html-checkbox-types '((unicode
                              (on . "&#x2611;")
                              (off . "&#x2610;")
                              (trans . "&#x2610;"))
                             (ascii
                              (on . "<code>[X]</code>")
                              (off . "<code>[&#xa0;]</code>")
                              (trans . "<code>[-]</code>"))
                             (html
                              (on . "<input type='checkbox' checked='checked'
onclick=\"return false;\"/>")
                              (off . "<input type='checkbox'
onclick=\"return false;\"/>")
                              (trans . "<input type='checkbox'
onclick=\"return false;\"/>")))
   org-html-link-home ""
   org-html-link-up ""
   org-html-postamble nil))

(setup ox-publish
  (:when-loaded
    (require 'lib-ox-publish))
  (:also-load ob)
  (:option*
   eli/blog-base-dir "~/Elilif.github.io/orgs"
   eli/blog-publish-dir "~/Elilif.github.io"
   eli/blog-sitamap "index.org"
   eli/blog-head "<link rel=\"icon\" href=\"/static/favion.png\">
<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/styles.css\"/>
<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/htmlize.css\" />
                  <script src=\"/scripts/script.js\"></script>
                  <script src=\"/scripts/toc.js\"></script>
<link href='https://unpkg.com/boxicons@2.1.4/css/boxicons.min.css' rel='stylesheet'>"
   eli/blog-preamble '(("en" "<nav class=\"nav\">
  <a href=\"/index.html\" class=\"button\">Home</a>
  <a href=\"/config.html\" class=\"button\">Literate Emacs Config</a>
  <a href=\"/rss.xml\" class=\"button\">RSS</a>
</nav>
<hr>"))
   eli/blog-postamble '(("en" "<hr class=\"Solid\">
<div class=\"info\">
  <span class=\"author\">Author: %a</span>
  <span class=\"author\">Email: %e</span>
  <span class=\"date\">Create Date: %d</span>
  <span class=\"date\">Last modified: %C</span>
  <span>Creator: %c</span>
</div>"))
   org-publish-project-alist
   `(("blog articles"
      :base-directory ,eli/blog-base-dir
      :publishing-directory ,(expand-file-name "articles" eli/blog-publish-dir)
      :base-extension "org"
      :recursive nil
      :htmlized-source t
      :headline-levels 4
      :publishing-function eli/org-blog-publish-to-html
      :exclude "rss.org"

      :auto-sitemap t
      :preparation-function eli/kill-sitemap-buffer
      :completion-function eli/blog-publish-completion
      :sitemap-filename ,eli/blog-sitamap
      :sitemap-title "Eli's Blog"
      :sitemap-sort-files anti-chronologically
      :sitemap-function eli/org-publish-sitemap
      :sitemap-format-entry eli/sitemap-dated-entry-format

      :html-head ,eli/blog-head
      :html-preamble t
      :html-preamble-format ,eli/blog-preamble
      :html-postamble t
      :html-postamble-format ,eli/blog-postamble
      :with-creator nil)
     ("blog rss"
      :preparation-function eli/kill-sitemap-buffer
      :publishing-directory ,eli/blog-publish-dir
      :base-directory ,eli/blog-base-dir
      :rss-extension "xml"
      :base-extension "org"
      :html-link-home "https://elilif.github.io/"
      :html-link-use-abs-url t
      :html-link-org-files-as-html t
      :include ("rss.org")
      :exclude ,eli/blog-sitamap

      :publishing-function eli/org-publish-rss-feed
      :auto-sitemap t
      :sitemap-function eli/org-publish-rss-sitemap
      :sitemap-title "Eli's Blog"
      :sitemap-filename "rss.org"
      :sitemap-sort-files anti-chronologically
      :sitemap-format-entry eli/org-publish-rss-entry)
     ("Eli's blog"
      :components ("blog articles" "blog rss"))
     ("Emacs config"
      :publishing-directory ,eli/blog-publish-dir
      :base-directory ,user-emacs-directory
      :include ("config.org")
      :exclude ".*.org"
      :publishing-function eli/org-blog-publish-to-html
      :html-head ,eli/blog-head
      :html-preamble t
      :html-preamble-format ,eli/blog-preamble
      :html-postamble t
      :html-postamble-format ,eli/blog-postamble)))
  (:after ox
    (add-to-list 'org-export-global-macros
                 '("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@"))
    (add-to-list 'org-export-global-macros
                 '("tags" . "@@html:<span class=\"tags\" data-tags=\"$1\"></span>@@"))
    (add-to-list 'org-export-global-macros
                 '("kbd" . "@@html:<kbd>$1</kbd>@@")))
  (:after ox-html
    (org-export-define-derived-backend 'blog 'html
      :translate-alist '((src-block . eli/org-blog-src-block)
                         (footnote-reference . eli/org-blog-footnote-reference)
                         (template . eli/org-blog-template)
                         (link . eli/org-blog-link))))
  (:advice
   ;; org-html--format-image :around eli/filter-org-html--format-image
   org-publish-find-date :override eli/org-publish-find-date)
  (:hooks
   org-export-before-processing-functions eli/org-export-src-babel-duplicate
   org-export-before-processing-functions eli/org-export-add-custom-id
   org-export-filter-src-block-functions eli/org-blog-add-noweb-ref
   org-export-filter-final-output-functions eli/org-blog-id-filter))


(provide 'init-blog)
;;; init-blog.el ends here.
