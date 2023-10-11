;;; lib-ox-publish.el --- blogs config -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

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

(defun eli/sitemap-dated-entry-format (entry style project)
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
              (format-time-string "%Y-%m-%d"
                                  (org-publish-find-date entry project))
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
         (pubdate (format-time-string
                   (cdr org-time-stamp-formats)
                   (org-publish-cache-set-file-property
                    file :date
                    (if (file-directory-p file)
                        (file-attribute-modification-time (file-attributes file))
                      (let ((date (org-publish-find-property file :date project)))
                        (cond
                         ((let ((ts (and (consp date) (assq 'timestamp date))))
                            (and ts
                                 (let ((value (org-element-interpret-data ts)))
                                   (and (org-string-nw-p value)
                                        (org-time-string-to-time value))))))
                         ((file-exists-p file)
                          (file-attribute-modification-time (file-attributes file)))
                         (t (error "No such file: \"%s\"" file)))))))))
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

(defun eli/publish-rss-papare (_project)
  (when-let ((rss-buffer (get-buffer "rss.org")))
    (kill-buffer rss-buffer)))

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
  (concat "#+TITLE: " title
          "\n"
          "#+DATE: 2023-10-10"
          "\n\n"
          (org-list-to-org list)))


(provide 'lib-ox-publish)
;;; lib-ox-publish.el ends here
