;;; lib-ox-publish.el --- blogs config -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

(cl-eval-when (compile)
  (require 'ox-rss))

;; (defun eli/filter-org-html--format-image (orig source attributes info)
;;   "Use base64 string instead of url to display images.

;; This functions is a advice for `org-html--format-image',
;; arguments, SOURCE ATTRIBUTES and INFO are like the arguments with
;; the same names of ORIG."
;;   (if-let* ((image-html (funcall orig source attributes info))
;;             (input-file (plist-get info :input-file))
;;             (blog-p (file-equal-p (file-name-directory input-file)
;;                                   eli/blog-base-dir))
;;             (image-base64 (format "data:image/%s+xml;base64,%s\"%s"
;;                                   (or (file-name-extension source) "")
;;                                   (base64-encode-string
;;                                    (with-temp-buffer
;;                                      (insert-file-contents-literally
;;                                       (file-relative-name
;;                                        (substring source 7)
;;                                        default-directory))
;;                                      (buffer-string)))
;;                                   (file-name-nondirectory source))))
;;       (replace-regexp-in-string "img src=\"\\(.*?\\)\"" image-base64 image-html
;;                                 nil nil 1)
;;     image-html))

(defun eli/org-html-htmlize-generate-font-lock-css ()
  "Create the CSS for all font-lock definitions in the current Emacs session."
  (interactive)
  (unless (require 'htmlize nil t)
    (error "htmlize library missing.  Aborting"))
  (and (get-buffer "*html*") (kill-buffer "*html*"))
  (with-temp-buffer
    (let ((fl (face-list))
          (htmlize-css-name-prefix "org-")
          (htmlize-output-type 'css)
          f)
      (while (setq f (pop fl))
        (when (and (symbolp f)
                   (string-match-p "^font-lock-" (symbol-name f)))
          (insert (org-add-props (copy-sequence "1") nil 'face f))))
      (htmlize-region (point-min) (point-max))))
  (pop-to-buffer-same-window "*html*")
  (goto-char (point-min))
  (when (re-search-forward "<style" nil t)
    (delete-region (point-min) (match-beginning 0)))
  (when (re-search-forward "</style>" nil t)
    (delete-region (1+ (match-end 0)) (point-max)))
  (beginning-of-line 1)
  (when (looking-at " +") (replace-match ""))
  (goto-char (point-min)))

(defvar eli/blog-tags nil)
(defun eli/sitemap-dated-entry-format (entry _style project)
  "Sitemap PROJECT ENTRY STYLE format that includes date."
  (let* ((file (org-publish--expand-file-name entry project))
         (parsed-title (org-publish-find-property file :title project))
         (title
          (if parsed-title
              (org-no-properties
               (org-element-interpret-data parsed-title))
            (file-name-nondirectory (file-name-sans-extension file))))
         (tags (org-publish-find-property file :filetags project))
         (tags-string (mapconcat
                       (lambda (tag)
                         (concat "#" tag))
                       tags " ")))
    (dolist (tag tags)
      (cl-pushnew tag eli/blog-tags :test #'string=))
    (org-publish-cache-set-file-property file :title title)
    (if (= (length title) 0)
        (format "*%s*" entry)
      (format "{{{timestamp(%s)}}}   [[file:%s][%s]] {{{tags(%s)}}}"
              (car (org-publish-find-property file :date project))
              (concat "articles/" entry)
              title
              tags-string))))

(defun eli/org-publish-find-date (file project)
  "Find the date of FILE in PROJECT.
This function assumes FILE is either a directory or an Org file.
If FILE is an Org file and provides a DATE keyword use it.  In
any other case use the file system's modification time.  Return
time in `current-time' format."
  (let ((file (org-publish--expand-file-name file project)))
    (or (org-publish-cache-get-file-property file :date nil t)
        (org-publish-cache-set-file-property
         file :date
         (if (file-directory-p file)
             (file-attribute-modification-time (file-attributes file))
           (let ((date (org-publish-find-property file :date project)))
             ;; DATE is a secondary string.  If it contains
             ;; a time-stamp, convert it to internal format.
             ;; Otherwise, use FILE modification time.
             (cond ((let ((ts (and (consp date) (assq 'timestamp date))))
                      (and ts
                           (let ((value (org-element-interpret-data ts)))
                             (and (org-string-nw-p value)
                                  (org-time-string-to-time value))))))
                   (date
                    (org-time-string-to-time (car date)))
                   ((file-exists-p file)
                    (file-attribute-modification-time (file-attributes file)))
                   (t (error "No such file: \"%s\"" file)))))))))

;; publishing
(defun eli/push-to-gitpage (&optional _args)
  (interactive)
  (start-process-shell-command "*publish*" nil "~/.emacs.d/private/shell.sh")
  (message "blogs deployed successfully!"))

(defun eli/blog-generate-sitemap (&optional _project)
  "Generate a sitemap.xml file for PROJTCT."
  (let* ((sitemap-path (file-name-concat eli/blog-publish-dir "sitemap.xml"))
         (base-url "https://elilif.github.io/")
         (files (directory-files-recursively eli/blog-publish-dir  ".html"))
         (sitemap-buffer (generate-new-buffer "*sitemap*")))
    (with-current-buffer sitemap-buffer
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">
")
      (dolist (file files)
        (insert
         (format "<url>\n<loc>%s</loc>\n<lastmod>%s</lastmod>\n</url>\n"
                 (concat base-url (file-relative-name file eli/blog-publish-dir))
                 (format-time-string "%Y-%m-%dT%H:%M:%S+08:00"
                                     (file-attribute-modification-time
                                      (file-attributes file))))))
      (insert "</urlset>")

      (write-region (point-min) (point-max) sitemap-path nil 3)
      (kill-buffer sitemap-buffer))))

(defun eli/blog-move-sitemap (project)
  (let* ((publishing-directory (plist-get project :publishing-directory))
         (sitamap (file-name-with-extension eli/blog-sitamap "html"))
         (orig-file (expand-file-name sitamap publishing-directory))
         (target-file (expand-file-name
                       sitamap
                       (file-name-directory publishing-directory))))
    (rename-file orig-file target-file t)))

(defun eli/blog-publish-completion (project)
  (eli/blog-move-sitemap project)
  (eli/blog-generate-sitemap)
  (setq org-html-head-extra ""
        eli/blog-tags nil))

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
         (abstract (eli/blog-get-abstract file))
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
            abstract
            (concat
             root
             link))))

(defun eli/kill-sitemap-buffer (project)
  (let* ((sitemap-filename (plist-get project :sitemap-filename))
         (base-dir (plist-get project :base-directory))
         (sitemap-filepath (expand-file-name sitemap-filename base-dir)))
    (when-let ((sitemap-buffer (find-buffer-visiting sitemap-filepath)))
      (kill-buffer sitemap-buffer))))

(defun eli/blog-get-abstract (file)
  "Get the contents of abstract block in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((beg (re-search-forward "^#\\+begin_abstract\n" nil t))
          (end (progn (re-search-forward "^#\\+end_abstract$" nil t)
                      (match-beginning 0))))
      (if beg
          (buffer-substring beg end)
        ""))))

(defun eli/org-publish-rss-feed (plist filename dir)
  "Publish PLIST to Rss when FILENAME is rss.org.
DIR is the location of the output."
  (if (equal "rss.org" (file-name-nondirectory filename))
      (org-publish-org-to
       'rss filename (concat "." org-rss-extension) plist dir)))

(defun eli/org-publish-sitemap (title list)
  "Generate the sitemap with title."
  (setq org-html-head-extra
        (format "<style>\n%s\n%s\n</style>"
                ".content:has([value=\"all\"]:checked) li{display: list-item;}\n"
                (mapconcat
                 (lambda (tag)
                   (format ".content:has([value=\"%s\"]:checked)
 li:has([data-tags~=\"%s\"]){display: list-item;}"
                           tag (concat "#" tag)))
                 eli/blog-tags "\n")))
  (concat "#+TITLE: " title
          "\n"
          "#+DATE: 2023-10-10"
          "\n"
          (format "#+BEGIN_EXPORT html
<section class=\"filter\">\n%s\n%s</section>
#+END_EXPORT"
                  "<label class=\"category\">
<input type=\"radio\" name=\"tag\" value=\"all\" checked/>
<span>All</span>
</label>"
                  (mapconcat
                   (lambda (tag)
                     (format "<label class=\"category\">
<input type=\"radio\" name=\"tag\" value=\"%s\"/>
<span>%s</span>
</label>"
                             tag tag))
                   eli/blog-tags "\n"))
          "\n"
          (org-list-to-org list)))

(defun eli/org-export-footnote-duplicate (backend)
  "Duplicate every footnote in the current buffer."
  (when (eq backend 'blog)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-footnote-re nil t)
        (let ((reference (org-element-context)))
          (when (eq 'footnote-reference (org-element-type reference))
            (let* ((ref (org-element-property :label reference))
                   (end (org-element-property :end reference))
                   (definition (nth 3 (org-footnote-get-definition ref))))
              (goto-char end)
              (insert "\n")
              (insert "#+begin_sidenotes")
              (insert "\n")
              (insert definition)
              (insert "\n")
              (insert "#+end_sidenotes")
              (insert "\n"))))))))

(defun eli/org-blog-add-noweb-ref (data backend _info)
  (when (eq backend 'blog)
    (replace-regexp-in-string
     "&lt;&lt;\\(.*?\\)&gt;&gt;"
     "<a href=\"#\\1\">\\1</a>"
     data nil nil 1)))

(defun eli/org-blog-id-filter (data backend _info)
  "Remove random ID attributes generated by Org."
  (when (memq backend '(blog rss))
    (replace-regexp-in-string
     " id=\"[[:alpha:]-]*org[[:alnum:]]\\{7\\}\""
     ""
     data t)))

(defun eli/org-export-src-babel-duplicate (backend)
  "Duplicate every src babels in the current buffer.

add \":noweb yes\" to duplicated src babels."
  (when (eq backend 'blog)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (let* ((end (copy-marker (match-end 0)))
               (string (match-string 0))
               (block (org-element-at-point))
               (code (org-element-property :value block))
               (params (org-element-property :parameters block)))
          (when (eq (org-element-type block) 'src-block)
            (goto-char (org-element-property :begin block))
            (insert "#+begin_multilang")
            (insert "\n")
            (goto-char end)
            (insert "\n")
            (when (and (string-match-p (org-babel-noweb-wrap) code)
                       (not (string-match-p ":noweb" params)))
              (insert string)
              (save-excursion
                (goto-char (1+ end))
                (end-of-line)
                (insert " :noweb yes"))
              (insert "\n"))
            (insert "#+end_multilang")
            (insert "\n")))))))

(defvar eli/blog-details-pair '("SUMMARY_BEG" . "SUMMARY_END"))

(defun eli/org-export-summary-convert (backend)
  ""
  (when (eq backend 'blog)
    (save-excursion
      (goto-char (point-min))
      (let ((details-regexp "^[ 	]*#\\+\\(%s\\):[ 	]*\\(%s\\)$"))
        (while (re-search-forward
                (format details-regexp
                        (car eli/blog-details-pair)
                        ".*")
                nil t)
          (when-let* ((summary (match-string 2))
                      (end (save-excursion
                             (re-search-forward
                              (format details-regexp
                                      (cdr eli/blog-details-pair)
                                      (regexp-quote summary))
                              nil t))))
            (setq end (copy-marker (match-beginning 0)))
            (insert "\n")
            (insert "#+begin_details\n")
            (insert "#+begin_summary\n")
            (insert summary)
            (insert "\n")
            (insert "#+end_summary\n")
            (insert "#+begin_detail")
            (goto-char end)
            (insert "#+end_detail\n")
            (insert "#+end_details\n")))))))

(defun eli/org-export-add-custom-id (backend)
  "Add CUSTOM-ID to headlines which dosen't have it."
  (when (eq backend 'blog)
    (org-map-entries
     (lambda ()
       (unless (org-entry-get (point) "CUSTOM_ID")
         (let* ((headline-name (replace-regexp-in-string
                                " " "-"
                                (nth 4 (org-heading-components)))))
           (org-entry-put (point) "CUSTOM_ID" headline-name)))))))

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
      (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
        (format "<div class=\"org-src-container\">\n%s%s\n</div>"
                ;; Build caption.
                (let ((caption (or (org-export-get-caption src-block)
                                   (org-element-property :name src-block))))
                  (if (not caption) ""
                    (let ((listing-number
                           (format
                            "<span class=\"listing-number\">%s </span>"
                            (format
                             (org-html--translate "Listing %d:" info)
                             (org-export-get-ordinal
                              src-block info nil #'org-html--has-caption-p)))))
                      (format "<label class=\"org-src-name\">%s%s</label>"
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
                  (format "<pre class=\"src src-%s\"%s>%s</pre>"
                          lang label code)))))))

(defun eli/org-blog-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((html-ext (plist-get info :html-extension))
         (dot (when (> (length html-ext) 0) "."))
         (link-org-files-as-html-maybe
          (lambda (raw-path info)
            ;; Treat links to `file.org' as links to `file.html', if
            ;; needed.  See `org-html-link-org-files-as-html'.
            (save-match-data
              (cond
               ((and (plist-get info :html-link-org-files-as-html)
                     (let ((case-fold-search t))
                       (string-match "\\(.+\\)\\.org\\(?:\\.gpg\\)?$" raw-path)))
                (concat (match-string 1 raw-path) dot html-ext))
               (t raw-path)))))
         (type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (org-string-nw-p desc))
         (path
          (cond
           ((member type '("http" "https" "ftp" "mailto" "news"))
            (url-encode-url (concat type ":" raw-path)))
           ((string= "file" type)
            ;; During publishing, turn absolute file names belonging
            ;; to base directory into relative file names.  Otherwise,
            ;; append "file" protocol to absolute file name.
            (setq raw-path
                  (org-export-file-uri
                   (org-publish-file-relative-name raw-path info)))
            ;; Possibly append `:html-link-home' to relative file
            ;; name.
            (let ((home (and (plist-get info :html-link-home)
                             (org-trim (plist-get info :html-link-home)))))
              (when (and home
                         (plist-get info :html-link-use-abs-url)
                         (file-name-absolute-p raw-path))
                (setq raw-path (concat (file-name-as-directory home) raw-path))))
            ;; Maybe turn ".org" into ".html".
            (setq raw-path (funcall link-org-files-as-html-maybe raw-path info))
            ;; Add search option, if any.  A search option can be
            ;; relative to a custom-id, a headline title, a name or
            ;; a target.
            (let ((option (org-element-property :search-option link)))
              (if (not option) raw-path
                (let ((path (org-element-property :path link)))
                  (concat raw-path
                          "#"
                          (org-publish-resolve-external-link option path t))))))
           (t raw-path)))
         (attributes-plist
          (org-combine-plists
           ;; Extract attributes from parent's paragraph.  HACK: Only
           ;; do this for the first link in parent (inner image link
           ;; for inline images).  This is needed as long as
           ;; attributes cannot be set on a per link basis.
           (let* ((parent (org-export-get-parent-element link))
                  (link (let ((container (org-export-get-parent link)))
                          (if (and (eq 'link (org-element-type container))
                                   (org-html-inline-image-p link info))
                              container
                            link))))
             (and (eq link (org-element-map parent 'link #'identity info t))
                  (org-export-read-attribute :attr_html parent)))
           ;; Also add attributes from link itself.  Currently, those
           ;; need to be added programmatically before `org-html-link'
           ;; is invoked, for example, by backends building upon HTML
           ;; export.
           (org-export-read-attribute :attr_html link)))
         (attributes
          (let ((attr (org-html--make-attribute-string attributes-plist)))
            (if (org-string-nw-p attr) (concat " " attr) ""))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'html info))
     ;; Image file.
     ((and (plist-get info :html-inline-images)
           (org-export-inline-image-p
            link (plist-get info :html-inline-image-rules)))
      (org-html--format-image path attributes-plist info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
        (if (not destination) desc
          (format "<a href=\"#%s\"%s>%s</a>"
                  (org-html--reference destination info)
                  attributes
                  desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
                             (org-export-resolve-fuzzy-link link info)
                           (org-export-resolve-id-link link info))))
        (pcase (org-element-type destination)
          ;; ID link points to an external file.
          (`plain-text
           (let ((fragment (concat "ID-" path))
                 ;; Treat links to ".org" files as ".html", if needed.
                 (path (funcall link-org-files-as-html-maybe
                                destination info)))
             (format "<a href=\"%s#%s\"%s>%s</a>"
                     path fragment attributes (or desc destination))))
          ;; Fuzzy link points nowhere.
          (`nil
           (format "<i>%s</i>"
                   (or desc
                       (org-export-data
                        (org-element-property :raw-link link) info))))
          ;; Link points to a headline.
          (`headline
           (let ((href (org-html--reference destination info))
                 ;; What description to use?
                 (desc
                  ;; Case 1: Headline is numbered and LINK has no
                  ;; description.  Display section number.
                  (if (and (org-export-numbered-headline-p destination info)
                           (not desc))
                      (mapconcat #'number-to-string
                                 (org-export-get-headline-number
                                  destination info) ".")
                    ;; Case 2: Either the headline is un-numbered or
                    ;; LINK has a custom description.  Display LINK's
                    ;; description or headline's title.
                    (or desc
                        (org-export-data
                         (org-element-property :title destination) info)))))
             (format "<a href=\"#%s\"%s>%s</a>" href attributes desc)))
          ;; Fuzzy link points to a target or an element.
          (_
           (if (and destination
                    (memq (plist-get info :with-latex) '(mathjax t))
                    (eq 'latex-environment (org-element-type destination))
                    (eq 'math (org-latex--environment-type destination)))
               ;; Caption and labels are introduced within LaTeX
               ;; environment.  Use "ref" or "eqref" macro, depending on user
               ;; preference to refer to those in the document.
               (format (plist-get info :html-equation-reference-format)
                       (org-html--reference destination info))
             (let* ((ref (org-html--reference destination info))
                    (org-html-standalone-image-predicate
                     #'org-html--has-caption-p)
                    (counter-predicate
                     (if (eq 'latex-environment (org-element-type destination))
                         #'org-html--math-environment-p
                       #'org-html--has-caption-p))
                    (number
                     (cond
                      (desc nil)
                      ((org-html-standalone-image-p destination info)
                       (org-export-get-ordinal
                        (org-element-map destination 'link #'identity info t)
                        info '(link) 'org-html-standalone-image-p))
                      (t (org-export-get-ordinal
                          destination info nil counter-predicate))))
                    (desc
                     (cond (desc)
                           ((not number) "No description for this link")
                           ((numberp number) (number-to-string number))
                           (t (mapconcat #'number-to-string number ".")))))
               (format "<a href=\"#%s\"%s>%s</a>" ref attributes desc)))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let ((fragment (concat "coderef-" (org-html-encode-plain-text path))))
        (format "<a href=\"#%s\" %s%s>%s</a>"
                fragment
                (format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, \
'%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
                        fragment fragment)
                attributes
                (format (org-export-get-coderef-format path desc)
                        (org-export-resolve-coderef path info)))))
     ;; External link with a description part.
     ((and path desc)
      (format "<a href=\"%s\"%s>%s</a>"
              (org-html-encode-plain-text path)
              attributes
              desc))
     ;; External link without a description part.
     (path
      (let ((path (org-html-encode-plain-text path)))
        (format "<a href=\"%s\"%s>%s</a>" path attributes path)))
     ;; No path, only description.  Try to do something useful.
     (t
      (format "<i>%s</i>" desc)))))

(defun eli/org-publish-get-wordcount (info)
  "Get the number of words in FILENAME."
  (let ((filename (plist-get info :input-file))
        (pub-dir (file-name-as-directory (plist-get info :publishing-directory)))
        (pub-func (plist-get info :publishing-function)))
    (if (org-publish-cache-file-needs-publishing filename pub-dir pub-func)
        (eli/org-publish-update-wordcount filename)
      (or (org-publish-cache-get (sha1 filename))
          (eli/org-publish-update-wordcount filename)))))

(defun eli/org-publish-update-wordcount (filename)
  (when (and (file-readable-p filename) (not (directory-name-p filename)))
    (let* ((org-inhibit-startup t)
           (visiting (find-buffer-visiting filename))
           (buffer (or visiting (find-file-noselect filename)))
           wordcount)
      (setq wordcount (unwind-protect
                          (with-current-buffer buffer
                            (org-count-words-buffer))
                        (unless visiting (kill-buffer buffer))))
      (org-publish-cache-set (sha1 filename) wordcount)
      (unless visiting (kill-buffer buffer))
      wordcount)))

(defvar eli/blog-status-format "<span><i class='bx bx-calendar'></i>
<span>%d</span></span>\n<span><i class='bx bx-edit'></i><span>%C</span></span>")
(defvar eli/blog-history-base-url "https://github.com/Elilif/Elilif.github.io/commits/master/orgs/")

(defun eli/blog-build-article-status (info)
  (let ((input-file (file-name-nondirectory (plist-get info :input-file))))
    (unless (string-equal input-file eli/blog-sitamap)
      (let ((spec (org-html-format-spec info))
            (history-url (concat eli/blog-history-base-url input-file)))
        (concat
         "<div class=\"post-status\">"
         (format-spec eli/blog-status-format spec)
         (format "<span><i class='bx bx-history'></i><span><a href=\"%s\">History</a></span></span>"
                 history-url)
         (format "<span><i class='bx bxs-hourglass'></i>%s 字</span>" (eli/org-publish-get-wordcount info))
         "</div>")))))

(defvar eli/blog-giscus-script "<script src=\"https://giscus.app/client.js\"
          data-repo=\"Elilif/Elilif.github.io\"
          data-repo-id=\"MDEwOlJlcG9zaXRvcnkyOTgxNjM5ODg=\"
          data-category=\"Announcements\"
          data-category-id=\"DIC_kwDOEcWfFM4Cdz5V\"
          data-mapping=\"pathname\"
          data-strict=\"0\"
          data-reactions-enabled=\"1\"
          data-emit-metadata=\"0\"
          data-input-position=\"top\"
          data-theme=\"light\"
          data-lang=\"zh-CN\"
          crossorigin=\"anonymous\"
          async>
  </script>")

(defun eli/blog-build-giscus (info)
  (let ((input-file (file-name-nondirectory (plist-get info :input-file))))
    (unless (string-equal input-file eli/blog-sitamap)
      eli/blog-giscus-script)))

(defun eli/org-blog-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let* ((xml-declaration (plist-get info :html-xml-declaration))
            (decl (or (and (stringp xml-declaration) xml-declaration)
                      (cdr (assoc (plist-get info :html-extension)
                                  xml-declaration))
                      (cdr (assoc "html" xml-declaration))
                      "")))
       (when (not (or (not decl) (string= "" decl)))
         (format "%s\n"
                 (format decl
                         (or (and org-html-coding-system
                                  ;; FIXME: Use Emacs 22 style here, see `coding-system-get'.
                                  (coding-system-get org-html-coding-system 'mime-charset))
                             "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
           (cond ((org-html-xhtml-p info)
                  (format
                   " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
                   (plist-get info :language) (plist-get info :language)))
                 ((org-html-html5-p info)
                  (format " lang=\"%s\"" (plist-get info :language))))
           ">\n")
   "<head>\n"
   (org-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
         (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format (plist-get info :html-home/up-format)
               (or link-up link-home)
               (or link-home link-up))))
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   (let ((div (assq 'content (plist-get info :html-divs))))
     (format "<%s id=\"%s\" class=\"%s\">\n"
             (nth 1 div)
             (nth 2 div)
             (plist-get info :html-content-class)))
   ;; Document title.
   (when (plist-get info :with-title)
     (let ((title (and (plist-get info :with-title)
                       (plist-get info :title)))
           (subtitle (plist-get info :subtitle))
           (html5-fancy (org-html--html5-fancy-p info)))
       (when title
         (format
          (if html5-fancy
              "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
            "<h1 class=\"title\">%s%s</h1>\n")
          (org-export-data title info)
          (if subtitle
              (format
               (if html5-fancy
                   "<p class=\"subtitle\" role=\"doc-subtitle\">%s</p>\n"
                 (concat "\n" (org-html-close-tag "br" nil info) "\n"
                         "<span class=\"subtitle\">%s</span>\n"))
               (org-export-data subtitle info))
            "")))))
   ;; add article status
   (eli/blog-build-article-status info)
   contents
   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; gisus
   (eli/blog-build-giscus info)
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Possibly use the Klipse library live code blocks.
   (when (plist-get info :html-klipsify-src)
     (concat "<script>" (plist-get info :html-klipse-selection-script)
             "</script><script src=\""
             org-html-klipse-js
             "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
             org-html-klipse-css "\"/>"))
   ;; Closing document.
   "</body>\n</html>"))

(defun eli/org-blog-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       (plist-get info :html-footnote-separator)))
   (let* ((n (org-export-get-footnote-number footnote-reference info))
          (id (format "fnr.%d%s"
                      n
                      (if (org-export-footnote-first-reference-p
                           footnote-reference info)
                          ""
                        ".100"))))
     (format
      (concat (plist-get info :html-footnote-format)
              "<input id=\"%s\" class=\"footref-toggle\" type=\"checkbox\">")
      (format "<label for=\"%s\" class=\"footref\">%s</label>"
              id n)
      id))))

(defvar eli/blog-static-dir "~/Elilif.github.io/static/post-img/")

(defun eli/blog-replace-static-path ()
  "Copy all static file links included in current file into
`eli/blog-static-dir'."
  (save-excursion
    (org-element-cache-reset)
    (let ((static-dir (file-name-as-directory
                       (file-name-concat eli/blog-static-dir
                                         (file-name-base
                                          (buffer-file-name)))))
          (datum (org-element-parse-buffer))
          links)
      (unless (file-exists-p static-dir)
        (make-directory static-dir t))
      (org-element-map datum 'link
        (lambda (link)
          (when (or (and (string= (org-element-property :type link) "file")
                         (org-file-image-p (org-element-property :path link)))
                    (string= (org-element-property :type link) "video"))
            (let* ((beg (org-element-property :begin link))
                   (end (org-element-property :end link))
                   (old-file (org-element-property :path link))
                   (raw-link (org-element-property :raw-link link))
                   (new-file (file-name-concat
                              ".."
                              (file-relative-name
                               static-dir
                               eli/blog-publish-dir)
                              (file-name-nondirectory old-file)))
                   (new-link (org-link-make-string
                              (replace-regexp-in-string old-file new-file raw-link))))
              (unless (file-exists-p new-file)
                (copy-file old-file new-file)
                (push (list (copy-marker beg)
                            (copy-marker end)
                            new-link)
                      links))))))
      (when (directory-empty-p static-dir)
        (delete-directory static-dir))
      (dolist (link links)
        (let ((beg (nth 0 link))
              (end (nth 1 link))
              (new-link (nth 2 link)))
          (goto-char beg)
          (delete-region beg end)
          (insert new-link))))
    (save-buffer)))

;;;###autoload
(defun eli/org-blog-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (with-current-buffer (or (find-buffer-visiting filename)
                           (find-file-noselect filename))
    (eli/blog-replace-static-path))
  (org-publish-org-to 'blog filename
                      (concat (when (> (length org-html-extension) 0) ".")
                              (or (plist-get plist :html-extension)
                                  org-html-extension
                                  "html"))
                      plist pub-dir))

(defun org-video-link-export (path _desc backend)
  (let ((ext (file-name-extension path))
        (file-name (file-name-base path)))
    (cond
     ((org-export-derived-backend-p backend 'html)
      (format "<video preload='metadata' controls='controls'>
<source type='video/%s' src='%s' />
<a href='%s'>[VIDEO: %s]</a>
</video>" ext path path file-name))
     ;; fall-through case for everything else
     (t
      path))))

(org-link-set-parameters "video" :export 'org-video-link-export)

(provide 'lib-ox-publish)
;;; lib-ox-publish.el ends here
