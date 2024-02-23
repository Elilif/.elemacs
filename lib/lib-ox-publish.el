;;; lib-ox-publish.el --- blogs config -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

(defun eli/filter-org-html--format-image (orig source attributes info)
  "Use base64 string instead of url to display images.

This functions is a advice for `org-html--format-image',
arguments, SOURCE ATTRIBUTES and INFO are like the arguments with
the same names of ORIG."
  (if-let* ((image-html (funcall orig source attributes info))
            (input-file (plist-get info :input-file))
            (blog-p (file-equal-p (file-name-directory input-file)
                                  eli/blog-base-dir))
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
                                nil nil 1)
    image-html))

(defun eli/sitemap-dated-entry-format (entry _style project)
  "Sitemap PROJECT ENTRY STYLE format that includes date."
  (let* ((file (org-publish--expand-file-name entry project))
         (parsed-title (org-publish-find-property file :title project))
         (title
          (if parsed-title
              (org-no-properties
               (org-element-interpret-data parsed-title))
            (file-name-nondirectory (file-name-sans-extension file)))))
    (org-publish-cache-set-file-property file :title title)
    (if (= (length title) 0)
        (format "*%s*" entry)
      (format "{{{timestamp(%s)}}}   [[file:%s][%s]]"
              (car (org-publish-find-property file :date project))
              (concat "articles/" entry)
              title))))

;; publishing
(defun eli/push-to-gitpage (&optional _args)
  (interactive)
  (start-process-shell-command "*publish*" nil "~/.emacs.d/private/shell.sh")
  (message "blogs deployed successfully!"))

(defun eli/blog-publish-completion (project)
  (let* ((publishing-directory (plist-get project :publishing-directory))
         (sitamap (file-name-with-extension eli/blog-sitamap "html"))
         (orig-file (expand-file-name sitamap publishing-directory))
         (target-file (expand-file-name
                       sitamap
                       (file-name-directory publishing-directory))))
    (rename-file orig-file target-file t)))

(defun eli/org-publish-rss-sitemap (title list)
  "Generate a sitemap of posts that is exported as a RSS feed.
TITLE is the title of the RSS feed.  LIST is an internal
representation for the files to include.  PROJECT is the current
project."
  (concat
   "#+TITLE: " title
   "\n\n"
   (org-list-to-subtree list)))

(defun eli/org-publish-rss-entry (entry _style project)
  "Format ENTRY for the posts RSS feed in PROJECT."
  (let* ((file (org-publish--expand-file-name entry project))
         (preview (eli/blog-get-preview file))
         (parsed-title (org-publish-find-property file :title project))
         (title
          (if parsed-title
              (org-no-properties
               (org-element-interpret-data parsed-title))
            (file-name-nondirectory (file-name-sans-extension file))))
         (root (org-publish-property :html-link-home project))
         (link (concat
                "articles/"
                (file-name-sans-extension entry) ".html"))
         (pubdate (car (org-publish-find-property file :date project))))
    (org-publish-cache-set-file-property file :title title)
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

(defun eli/kill-sitemap-buffer (project)
  (let* ((sitemap-filename (plist-get project :sitemap-filename))
         (base-dir (plist-get project :base-directory))
         (sitemap-filepath (expand-file-name sitemap-filename base-dir)))
    (when-let ((sitemap-buffer (find-buffer-visiting sitemap-filepath)))
      (kill-buffer sitemap-buffer))))

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

(defun eli/org-publish-sitemap (title list)
  "Generate the sitemap with title."
  (concat "#+TITLE: " title
          "\n"
          "#+DATE: 2023-10-10"
          "\n\n"
          (org-list-to-org list)))


(defun eli/org-export-src-babel-duplicate (backend)
  "Duplicate every src babels in the current buffer.

add \":noweb yes\" to duplicated src babels."
  (when (eq backend 'blog)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (let ((end (copy-marker (match-end 0)))
              (string (match-string 0))
              (block (org-element-at-point)))
          (goto-char (org-element-property :begin block))
          (insert "#+begin_multilang")
          (insert "\n")
          (goto-char end)
          (insert "\n")
          (insert string)
          (save-excursion
            (goto-char (1+ end))
            (end-of-line)
            (insert " :noweb yes"))
          (insert "\n")
          (insert "#+end_multilang"))))))

(defun eli/org-blog-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((lang (org-element-property :language src-block))
           (code (org-html-format-code src-block info))
           (label (let ((lbl (org-html--reference src-block info t)))
                    (if lbl (format " id=\"%s\"" lbl) "")))
           (klipsify  (and  (plist-get info :html-klipsify-src)
                            (member lang '("javascript" "js"
                                           "ruby" "scheme" "clojure" "php" "html")))))
      (if (not lang) (format "<pre class=\"example\"%s><code>\n%s</code></pre>" label code)
        (format "<div class=\"org-src-container\">\n%s%s\n</div>"
                ;; Build caption.
                (let ((caption (or (org-export-get-caption src-block)
                                   (org-element-property :name src-block))))
                  (if (not caption) ""
                    (let ((listing-number
                           (format
                            "<span class=\"listing-number\">%s </span>"
                            "Label: ")))
                      (format "<div class=\"org-src-name\">%s%s</div>"
                              listing-number
                              (org-trim (org-export-data caption info))))))
                ;; Contents.
                (if klipsify
                    (format "<pre><code class=\"src src-%s\"%s%s>%s</code></pre>"
                            lang
                            label
                            (if (string= lang "html")
                                " data-editor-type=\"html\""
                              "")
                            code)
                  (format "<pre class=\"src src-%s\"%s><code>%s</code></pre>"
                          lang label code)))))))

;;;###autoload
(defun eli/org-blog-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'blog filename
                      (concat (when (> (length org-html-extension) 0) ".")
                              (or (plist-get plist :html-extension)
                                  org-html-extension
                                  "html"))
                      plist pub-dir))

(provide 'lib-ox-publish)
;;; lib-ox-publish.el ends here
