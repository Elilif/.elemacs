;; init-bib.el --- Initialize bib configurations.	-*- lexical-binding: t -*-

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

(setq eli/bibliography '("/home/eli/Documents/Books/catalog.bib"))

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
  (defun eli/citar-org-format-note (key entry)
    "Format a note from KEY and ENTRY."
    (let* ((template (citar--get-template 'note))
           (note-meta (when template
			(citar-format--entry template entry)))
           (filepath (expand-file-name
                      (concat key ".org")
                      (car citar-notes-paths)))
           (buffer (find-file filepath)))
      (with-current-buffer buffer
	;; This just overrides other template insertion.
	(erase-buffer)
	(citar-org-roam-make-preamble key)
	(insert "#+title: ")
	(when template (insert (replace-regexp-in-string ":/home.*:PDF" (car (gethash key (citar-get-files key))) note-meta))))))

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
  (setq citar-note-format-function #'eli/citar-org-format-note)
  (setq citar-notes-paths '("~/Dropbox/org/roam/references")))

(with-eval-after-load 'citar
  (citar-org-roam-mode)
  (setq citar-org-roam-note-title-template (cdr (assoc 'note citar-templates)))
  (defun citar-org-roam--create-capture-note (citekey entry)
    "Open or create org-roam node for CITEKEY and ENTRY."
    ;; adapted from https://jethrokuan.github.io/org-roam-guide/#orgc48eb0d
    (let ((title (citar-format--entry
                  citar-org-roam-note-title-template entry)))
      (org-roam-capture-
       :templates
       '(("r" "reference" plain "%?" :if-new
          (file+head
           "%(concat
 (when citar-org-roam-subdir (concat citar-org-roam-subdir \"/\")) \"${citekey}.org\")"
           "#+title: ${title}\n")
          :immediate-finish t
          :unnarrowed t))
       :info (list :citekey citekey)
       :node (org-roam-node-create :title (replace-regexp-in-string ":/home.*:PDF" (car (gethash citekey (citar-get-files citekey))) title))
       :props '(:finalize find-file))
      (org-roam-ref-add (concat "@" citekey)))))

(with-eval-after-load 'citar
  (citar-embark-mode))


(with-eval-after-load 'calibredb
  (setq calibredb-root-dir "~/Documents/Books")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Documents/Books")))
  (defun eli/update-calibre-bibtex ()
    "Export the catalog with BibTex file."
    (interactive)
    (calibredb-command :command "catalog"
                       :option (format "%s"
                                       (shell-quote-argument
					(expand-file-name
					 (or calibredb-ref-default-bibliography
                                             (concat (file-name-as-directory calibredb-root-dir) "catalog.bib")))))
                       :input (s-join " " (-remove 's-blank? (-flatten "--fields title,authors,formats,isbn,pubdate,publisher,tags,languages")))
                       :library (format "--library-path %s" (calibredb-root-dir-quote)))
    (calibredb-ref-default-bibliography)
    (message "Updated BibTex file."))

  (defun eli/calibre-refresh ()
    (setq calibredb-ref-default-bibliography (concat (file-name-as-directory calibredb-root-dir) "catalog.bib"))
    )
  (add-hook 'calibredb-search-mode-hook 'eli/calibre-refresh))

(with-eval-after-load 'bibtex-completion
  (setq bibtex-completion-bibliography eli/bibliography
	bibtex-completion-library-path "/home/eli/Documents/Thesis"
	bibtex-completion-notes-path "/home/eli/Dropbox/org/roam/references"
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


(provide 'init-bib)
;;; init-bib.el ends here.
