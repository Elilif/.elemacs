;;; lib-burly.el --- burly config -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

(defun eli/org-agenda-bookmark-make-record ()
  "This implements the `bookmark-make-record-function' type (which see)
for org-agenda."
  (let* ((bookmark-name (buffer-name)))
    `(,bookmark-name
      (handler . eli/org-agenda-bookmark-handler))))

(defun eli/org-agenda-bookmark-handler (_bmk)
  "This implements the `handler' function interface for the record
type returned by `eli/org-agenda-bookmark-make-record', which see."
  (require 'org)
  (org-agenda nil "g"))

(defun eli/burly-rename-tab (name)
  (tab-bar-rename-tab (burly-tabs--abbreviate-name name)))

;;;###autoload
(defun eli/burly-bookmark-handler (bookmark)
  "Handler function for Burly BOOKMARK."
  (let ((previous-name burly-opened-bookmark-name))
    ;; Set opened bookmark name before actually opening it so that the
    ;; tabs-mode advice functions can use it beforehand.
    (setf burly-opened-bookmark-name (car bookmark))
    (condition-case err
        (progn
          (burly-open-url (alist-get 'url (bookmark-get-bookmark-record bookmark)))
          (mapc #'find-file
                (alist-get 'files (bookmark-get-bookmark-record bookmark))))
      (error (setf burly-opened-bookmark-name previous-name)
             (signal (car err) (cdr err))))))

(defun eli/burly-bookmark-names ()
  "Return list of all Burly bookmark names."
  (bookmark-maybe-load-default-file)
  (cl-loop for bookmark in bookmark-alist
           for (_name . params) = bookmark
           when (memq (alist-get 'handler params)
                      '(burly-bookmark-handler
                        eli/burly-bookmark-handler))
           collect (car bookmark)))

;;;###autoload
(defun eli/burly-bookmark-windows (name)
  "Bookmark the current frame's window configuration and files as NAME."
  (interactive
   (list (completing-read "Save Burly bookmark: " (burly-bookmark-names)
                          nil nil burly-bookmark-prefix)))
  (let* ((files (tabspaces--store-buffers (tabspaces--buffer-list)))
         (record (list (cons 'url (burly-windows-url))
                       (cons 'handler #'eli/burly-bookmark-handler)
                       (cons 'files files))))
    (bookmark-store name record nil)))


(provide 'lib-burly)
;;; lib-burly.el ends here
