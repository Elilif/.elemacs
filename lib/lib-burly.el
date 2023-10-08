;;; lib-burly.el --- burly config -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

(defun eli/org-agenda-bookmark-make-record ()
  "This implements the `bookmark-make-record-function' type (which see)
for org-agenda."
  (let* ((bookmark-name (buffer-name)))
    `(,bookmark-name
      ,@(bookmark-make-record-default 'no-file)
      (handler . eli/org-agenda-bookmark-handler))))

(defun eli/org-agenda-bookmark-handler (bmk)
  "This implements the `handler' function interface for the record
type returned by `eli/org-agenda-bookmark-make-record', which see."
  (require 'org)
  (org-agenda nil "g")
  (let* ((buf org-agenda-buffer-name))
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bmk)))))

(defun eli/burly-rename-tab (name)
  (tab-bar-rename-tab (burly-tabs--abbreviate-name name)))



(provide 'lib-burly)
;;; lib-burly.el ends here
