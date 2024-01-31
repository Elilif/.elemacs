;;; lib-citar.el --- citar config -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

(require 'nerd-icons)

(defvar citar-indicator-files-icons
  (citar-indicator-create
   :symbol (nerd-icons-codicon
            "nf-cod-file"
            :face 'nerd-icons-green)
   :function #'citar-has-files
   :padding "  " ; need this because the default padding is too low for these icons
   :tag "has:files"))

(defvar citar-indicator-links-icons
  (citar-indicator-create
   :symbol (nerd-icons-codicon
            "nf-cod-link"
            :face 'nerd-icons-orange)
   :function #'citar-has-links
   :padding "  "
   :tag "has:links"))

(defvar citar-indicator-notes-icons
  (citar-indicator-create
   :symbol (nerd-icons-mdicon
            "nf-md-notebook_outline"
            :face 'nerd-icons-blue)
   :function #'citar-has-notes
   :padding "  "
   :tag "has:notes"))

(defvar citar-indicator-cited-icons
  (citar-indicator-create
   :symbol (nerd-icons-faicon
            "nf-fa-circle_o"
            :face 'nerd-icons-green)
   :function #'citar-is-cited
   :padding "  "
   :tag "is:cited"))

;; search pdf contents
(defun eli/search-pdf-contents (keys-entries &optional str)
  "Search pdf contents.

KEYS-ENTRIES should be either a list citar KEYS or a single key.
STR is the searching string."
  (interactive (list (citar-select-refs)))
  (let ((files (seq-filter
                (lambda (file)
                  (member (file-name-extension file) '("pdf")))
                (mapcar (lambda (key)
                          (car (gethash key (citar-get-files key))))
                        keys-entries)))
        (search-str (or str (read-string "Search string: "))))
    (pdf-occur-search files search-str t)))

;; with this, you can exploit embark's multitarget actions, so that you can run `embark-act-all`
(add-to-list 'embark-multitarget-actions #'ex/search-pdf-contents)

(provide 'lib-citar)
;;; lib-citar.el ends here
