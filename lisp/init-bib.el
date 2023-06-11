;; init-bib.el --- Initialize init-bib configurations.	-*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 by Eli

;; Author: Eli <eli.q.qian@gmail.com>
;; URL: https://github.com/Elilif/.elemacs

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

;;; TODO: refactor

(cl-eval-when (compile)
  (require 'oc-csl)
  (require 'citar)
  (require 'bibtex)
  (require 'bibtex-completion))

(setq eli/bibliography '("/home/eli/Documents/Mybooks/Books.bib"))

(with-eval-after-load 'org
  (require 'oc-csl)
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar)
  (setq org-cite-global-bibliography eli/bibliography)
  (setq org-cite-csl-styles-dir "~/Documents/styles")
  (setq org-cite-export-processors '((beamer natbib)
    			                     (latex biblatex)
    			                     (t csl)))
  (require 'oc-natbib)
  (require 'oc-biblatex))

(with-eval-after-load 'citar
  (setq citar-templates
		'((main . "${author:30}     ${date year issued:4}     ${title:48}")
		  (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
		  (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
		  (note . "${title}
#+filetags: :book:
- bibliography ::
- tags :: ${tags}
- keywords :: ${keywords}

* Notes
:PROPERTIES:
:Custom_ID: ${=key=}
:URL: ${url}
:AUTHOR: ${author}
:NOTER_DOCUMENT: ${file}
:NOTER_PAGE:
:NOANKI: t
:END:")))
  (setq citar-symbols
		`((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
		  (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
		  (link ,(all-the-icons-octicon "link" :face
										'all-the-icons-orange :v-adjust 0.01) . " ")))
  (setq citar-symbol-separator "  ")
  (setq citar-bibliography eli/bibliography)
  (setq citar-at-point-function 'citar-dwim)
  (setq citar-notes-paths '("~/Dropbox/org/roam/books"))
  (setq citar-library-paths '("~/Documents/Mybooks/"))

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
  (add-to-list 'embark-multitarget-actions #'ex/search-pdf-contents))

(with-eval-after-load 'citar
  (citar-org-roam-mode)
  (setq citar-org-roam-note-title-template (cdr (assoc 'note citar-templates))
		citar-org-roam-subdir "books"))

(with-eval-after-load 'citar
  (citar-embark-mode))

(with-eval-after-load 'bibtex-completion
  (setq bibtex-completion-bibliography eli/bibliography
		bibtex-completion-library-path "/home/eli/Documents/Thesis"
		bibtex-completion-notes-path "/home/eli/Dropbox/org/roam/books"
		bibtex-completion-pdf-field "file"
		;; bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

		bibtex-completion-additional-search-fields '(keywords)
		bibtex-completion-display-formats
		'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
		  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
		  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
		  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
		  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))))

(with-eval-after-load 'bibtex
  (setq bibtex-autokey-year-length 4
		bibtex-autokey-name-year-separator "-"
		bibtex-autokey-year-title-separator "-"
		bibtex-autokey-titleword-separator "-"
		bibtex-autokey-titlewords 2
		bibtex-autokey-titlewords-stretch 1
		bibtex-autokey-titleword-length 5
		bibtex-dialect 'biblatex
		)
  (setq my/primary-bibtex-biblatex-entry-alist
		'(("MastersThesis" "MastersThesis"
           (("author")
			("title")
			("school")
			("year")
			("tertiaryauthor")
			("keywords")
			("abstract")
			("databaseprovider")
			("url"))
           nil
           nil)))
  (setq bibtex-biblatex-entry-alist
		(append bibtex-biblatex-entry-alist my/primary-bibtex-biblatex-entry-alist)))



;;;; provide
(provide 'init-bib)
;;; init-bib.el ends here.
