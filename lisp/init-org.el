;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-

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

(cl-eval-when (compile)
  (require 'embark)
  (require 'server)
  (require 'mixed-pitch)
  (require 'org-roam)
  (require 'svg-tag-mode)
  (require 'lib-svg-tag-mode)
  (require 'lib-ox)
  (require 'lib-org-anki)
  (require 'lib-org-media-note)
  (require 'lib-appt)
  (require 'lib-org-roam)
  (require 'lib-org)
  (require 'lib-org-agenda)
  (require 'lib-org-capture))

;;;; org better than default
(setup org
  (:also-load lib-org)
  (:hooks org-cycle-hook org-cycle-hide-drawers
		  org-after-todo-statistics-hook eli/org-summary-todo)
  (:advice org-cycle-hide-drawers :override elemacs/org-cycle-hide-drawers
		   org-return :around eli/org-return-wrapper
		   org-do-emphasis-faces :override eli/org-do-emphasis-faces
		   org-element--parse-generic-emphasis :override eli/org-element--parse-generic-emphasis
		   fill-move-to-break-point :before eli/adjust-line-break-point)
  (:bind
   "C-<tab>" eli/org-expand-all))

;;;; org
(setup org
  (:iload calendar find-func format-spec org-macs org-compat
		  org-faces org-entities org-list org-pcomplete org-src
		  org-footnote org-macro ob org org-clock org-agenda
		  org-capture)
  (:option*
   org-modules '(ol-bbdb ol-bibtex ol-info)
   org-indirect-buffer-display 'current-window
   org-export-preserve-breaks nil
   org-adapt-indentation t
   org-blank-before-new-entry '((heading . nil)
                                (plain-list-item . auto))
   org-fontify-quote-and-verse-blocks t
   org-link-context-for-files 1
   org-link-frame-setup '((vm . vm-visit-folder-other-frame)
						  (vm-imap . vm-visit-imap-folder-other-frame)
						  (gnus . org-gnus-no-new-news)
						  (file . find-file)
						  (wl . wl-other-frame))

   org-id-link-to-org-use-id 'create-if-interactive
   org-use-fast-todo-selection 'expert
   org-log-into-drawer t
   org-log-done 'time
   org-custom-properties '("CUSTOM_ID")
   org-startup-folded t
   org-hide-block-startup t
   org-hide-emphasis-markers t
   org-startup-with-inline-images t
   org-footnote-auto-adjust t
   org-todo-keywords
   (quote ((sequence "TODO(t/!)" "STARTED(s)" "|" "DONE(d!/!)")
		   (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
		   (sequence "WAITING(w@/!)" "NEXT(n!/!)"
                     "SOMEDAY(S)" "|" "CANCELLED(c@/!)")))
   org-ellipsis "▼")
  (:advice
   buffer-substring--filter :filter-return eli/unfill-string)
  (:hook
   org-indent-mode
   auto-fill-mode)
  (:bind 
   "<f8>" eli/rating
   "<f7>" eli/entry-rating
   "<f9>" eli/set-film-ratings
   "<f10>" eli/get-film-rating
   "C-c C-l" ar/org-insert-link-dwim)
  (:global
   "C-c l" org-store-link
   "C-c c" org-capture))

;;;; org-agenda
(setup org-agenda
  (:once (list :before 'org-agenda
			   :packages 'org-agenda)
	(require 'lib-org-agenda))
  (:option* org-agenda-clock-consistency-checks '(:max-duration "10:00" :min-duration 0 :max-gap "0:00"
																:gap-ok-around ("4:00") :default-face ((:background "DarkRed")
																									   (:foreground "white"))
																:overlap-face nil :gap-face nil
																:no-end-time-face nil :long-face nil :short-face nil)
			org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2 :fileskip0 t
														   :sort (3 . 84) :formula %)
			org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2 :fileskip0 t
														   :sort (3 . 84) :formula %)
			org-agenda-span 'day
			org-agenda-show-inherited-tags nil
			org-agenda-window-setup 'only-window
			org-agenda-log-mode-items '(clock)
			org-agenda-log-mode-add-notes nil
			org-agenda-files '("~/Dropbox/org/journal.org"
							   "/home/eli/Dropbox/org/Français.org"
							   "/home/eli/Dropbox/org/daily.org"
							   "/home/eli/Dropbox/org/lists.org"
							   "/home/eli/Dropbox/org/inbox.org"
							   "/home/eli/Dropbox/org/words.org"
							   "/home/eli/Dropbox/org/projects.org")
			org-agenda-custom-commands '(("g" "GTD"
										  ((agenda ""
												   ((org-agenda-overriding-header
													 (concat "Day-agenda (W"
															 (format-time-string "%U" (current-time))
															 ") "
															 (eli/make-progress 50)))))
										   (tags-todo  "/+TODO"
													   ((org-agenda-overriding-header
														 (propertize "Inbox" 'face 'mindre-faded))
														(org-tags-match-list-sublevels t)
														(org-agenda-skip-function
														 '(org-agenda-skip-entry-if 'timestamp))))
										   (tags-todo "/+NEXT"
													  ((org-agenda-overriding-header "Next")))
										   (tags-todo "PROJECT|INBOX/+STARTED"
													  ((org-agenda-overriding-header "Started")))
										   (tags-todo "/+PROJECT"
													  ((org-agenda-overriding-header "Projects")))
										   (tags-todo "/+WAITING"
													  ((org-agenda-overriding-header "Waiting")))
										   (tags-todo "/+SOMEDAY"
													  ((org-agenda-overriding-header "Someday/Maybe")))))))
  (:advice
   org-agenda-clock-goto :around eli/org-agenda-goto-started-task
   org-agenda-log-mode :after (lambda (&rest _arg) (goto-char (point-min)))
   org-agenda-redo-all :after (lambda (&rest _arg) (goto-char (point-min)))
   org-agenda-files :filter-return dynamic-agenda-files-advice)
  (:hooks org-agenda-finalize-hook eli/show-progress-color
		  org-after-todo-state-change-hook update-dynamic-agenda-hook))

;;;; org-table
(setup org-duration
  (:option*
   org-duration-format '((special . h:mm))))

;;;; org-capture
(setup org-capture
  (:also-load lib-org-capture)
  (:option*
   org-agenda-dir "~/Dropbox/org"
   org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir)
   org-agenda-file-projects (expand-file-name "projects.org" org-agenda-dir)
   org-agenda-file-habit (expand-file-name "daily.org" org-agenda-dir)
   org-agenda-file-notes (expand-file-name "notes.org" org-agenda-dir)
   org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir)
   org-agenda-file-te (expand-file-name "words.org" org-agenda-dir)
   org-agenda-file-lists (expand-file-name "lists.org" org-agenda-dir)
   org-capture-templates'(("t" "Todo" entry (file org-agenda-file-inbox)
						   "* TODO %?\n\n%i\n%U"
						   :empty-lines 0)
						  ("c" "Start-new" entry (file org-agenda-file-inbox)
						   "* TODO %i"
						   :empty-lines 0
						   :immediate-finish t)
						  ("1" "global notes" entry (file+headline org-agenda-file-inbox "Notes")
						   "* %i"
						   :prepend t
						   :empty-lines 0
						   :immediate-finish t)
						  ("2" "global todo" entry (file org-agenda-file-inbox)
						   "* TODO %i"
						   :empty-lines 0
						   :immediate-finish t)
						  ("p" "Project" entry (file org-agenda-file-projects)
						   "* PROJECT %?"
						   :empty-lines 0)
						  ("h" "Habit" entry (file org-agenda-file-habit)
						   "* TODO %?\nSCHEDULED: <%(org-read-date nil nil \"+0d\") .+1d>\
\n:PROPERTIES:\n:STYLE:    habit\n:END:\n\n%U"
						   :empty-lines 0)
						  ("n" "Notes" entry (file+headline org-agenda-file-inbox "Notes")
						   "* %?\n%(v-i-or-nothing)\n%(v-a-or-nothing)\n%U"
						   :empty-lines 0
						   :prepend t)
						  ("j" "Journals" entry (file+function
												 org-agenda-file-journal
												 org-reverse-datetree-goto-date-in-file)
						   "* %<%H:%M> %?"
						   :empty-lines 1
						   :prepend t
						   :clock-resume t
						   :clock-in t
						   )
						  ("e" "Events" entry (file+function
											   "~/Elilif.github.io/Eli's timeline.org"
											   (lambda ()
												 (eli/org-capture-template-goto-today
												  "%Y-%m-%d" 0 10 361)))
						   "* %?"
						   )
						  ("B" "Blogs" plain (file eli/capture-report-date-file)
						   "#+TITLE: %?\n#+DATE: %<%Y-%m-%d>\n#+STARTUP: showall\
\n#+OPTIONS: H:2 num:2\n#+HTML_HEAD_EXTRA: <style> .figure p {text-align: center;}</style>\n"
						   )
						  ("T" "Time Report" plain (file+function
													"~/Dropbox/org/Clock_Report.org"
													org-reverse-datetree-goto-date-in-file)
						   "#+BEGIN: clocktable :scope agenda-with-archives :maxlevel 6 \
:block %<%Y-%m-%d> :fileskip0 t \
:indent t :link t :formula % :sort (3 . ?T)\n#+END:"
						   :empty-lines 0
						   :jump-to-captured t)
						  ("d" "Digests" entry (file+olp+datetree org-agenda-file-notes)
						   "* %a\n%?\n%(v-i-or-nothing)\n%U"
						   :empty-lines 0)
						  ("w" "Words" checkitem (file+function
												  org-agenda-file-te
												  (lambda ()
													(eli/org-capture-template-goto-today
													 "TODO %Y-%m-%d [/]" 5 15 114)))
						   "[ ] %(v-i-or-nothing-word)%?"
						   :prepend t)
						  ("f" "Français" entry (file "~/Dropbox/org/Français.org")
						   "* TODO %u [/]\n%?"
						   :jump-to-captured t)
						  ("g" "古文" entry (file "~/Dropbox/org/古文.org")
						   "* TODO %u [/]\n%?"
						   :jump-to-captured t)
						  ("b" "Book" entry (file+headline org-agenda-file-lists "Books")
						   "* %?\n  %^{Title}p %^{Isbn}p %^{Types}p %^{Authors}p %^{Translator}p\
  %^{Publisher}p %^{Nation}p %^{Lang}p %^{Rating}p"
						   :prepend t)
						  ("m" "Movies and Musicals" entry (file+headline
															org-agenda-file-lists
															"Movies and Musicals")
						   "* %?\n %^{Title}p %^{IMDB}p %^{URL}p %^{Director}p %^{Writer}p\
 %^{Actors}p %^{Types}p %^{Time}p %^{Release}p %^{Nation}p %^{Lang}p %^{Rating}p"
						   :prepend t)
						  ("s" "Series" entry (file+headline org-agenda-file-lists "Series")
						   "* %?\n %^{Title}p %^{IMDB}p %^{URL}p %^{Director}p %^{Writer}p\
 %^{Actors}p %^{Types}p %^{Time}p %^{Episodes}p\
 %^{Release}p %^{Nation}p %^{Lang}p %^{Rating}p"
						   :prepend t)
						  ("a" "Animes" entry (file+headline org-agenda-file-lists "Animes")
						   "* %?\n %^{Title}p %^{URL}p %^{Episodes}p %^{Release}p\
 %^{Director}p %^{Authors}p %^{Publisher}p %^{Rating}p"
						   :prepend t)
						  ("r" "NOTE" entry (file "~/Dropbox/org/roam/inbox.org")
						   "* %?\n%(v-i-or-nothing)\n%(v-a-or-nothing)"
						   :create-id t)))
  (:hooks
   org-capture-prepare-finalize-hook eli/org-capture-maybe-create-id
   org-capture-prepare-finalize-hook eli/fill-quote-and-checklist))

;;;; org-cycle
(setup org-cycle
  (:option*
   org-cycle-inline-images-display t))

;;;; org-element
(setup org-element
  (:option*
   org-element-cache-persistent nil
   org-element--cache-self-verify-frequency 0.0001))

;;;; org-persist
(setup org-persist
  (:option*
   org-persist-directory "~/.emacs.d/.cache/org-persist/"))

;;;; org-babel
(setup org
  (:once (list :before 'org-insert-structure-template)
	(:option*
	 org-confirm-babel-evaluate nil
	 org-babel-default-header-args '((:session . "none")
									 (:results . "output replace")
									 (:exports . "code")
									 (:cache . "no")
									 (:noweb . "no")
									 (:hlines . "no")
									 (:tangle . "no"))
	 org-babel-load-languages '((emacs-lisp . t)
								(shell . t)
								(C . t)
								(latex . t)))
	(org-babel-do-load-languages 'org-babel-load-languages
								 '((emacs-lisp . t)
								   (shell . t)
								   (C . t)
								   (latex . t)))))

;;;; org-clock
(setup org-clock
  (:option*
   org-clock-in-switch-to-state 'eli/clock-in-to-nest
   org-clock-mode-line-total 'today
   org-clock-out-remove-zero-time-clocks t
   org-clock-continuously t
   org-clock-sound "~/.emacs.d/private/bellring.wav"
   ))

;;;; org-protocol
(setup org-protocol
  (:once (list :hooks 'org-mode-hook)
	(run-with-idle-timer 2 nil
						 (lambda ()
						   (require 'org-protocol)
						   (require 'server)
						   (unless (server-running-p)
							 (server-start))))))

;;;; org-refile
(setup org-file
  (:option*
   org-refile-use-outline-path 'file
   org-outline-path-complete-in-steps nil
   org-refile-targets
   '((nil :maxlevel . 5)
     (org-agenda-files :maxlevel . 5)
     (("~/Dropbox/org/Clock_Report.org_archive") :maxlevel . 1))))

;;;; org-habit
(setup org-habit
  (:with-feature org-agenda
	(:also-load org-habit
				lib-org-habit))
  (:option*
   org-habit-graph-column 1
   org-habit-preceding-days 10
   org-habit-following-days 1
   org-habit-show-habits-only-for-today nil))

;;;; prettify symbol
(setup org
  (:hook
   (lambda ()
	 (setq prettify-symbols-alist '(("#+BEGIN_SRC" . "✎")
									("#+END_SRC" . "□")
									("#+begin_src" . "✎")
									("#+end_src" . "□")
									("#+RESULTS:" . "⟾")
									("[ ]" . "☐")
									("[-]" . "🝕")
									("[X]" . "🗹")
									("#+begin_quote" . "»")
									("#+end_quote" . "□")
									("#+begin_verse" . "ζ")
									("#+end_verse" . "□")
									("#+begin_example" . "⟝")
									("#+end_example" . "□")
									("#+begin_export" . "🙵")
									("#+end_export" . "□")
									("#+END:" . "□")
									("#+BEGIN:" . "✎")
									("#+CAPTION:" . "✑")
									("#+ATTR_LATEX" . "🄛")
									))
	 (prettify-symbols-mode)
	 )))


;;;; org-superstar
(setup org-superstar
  (:option*
   org-superstar-headline-bullets-list '("⦿" "⊚" "𐰧" "◯" "●" "►" "▻")
   org-superstar-prettify-item-bullets nil)
  (:hook-into org-mode))

;;;; org-roam
(setup org-roam
  (:also-load lib-org-roam)
  (:option*
   org-roam-directory "~/Dropbox/org/roam/"
   org-roam-db-gc-threshold most-positive-fixnum
   org-roam-mode-sections (list #'org-roam-backlinks-section
								#'org-roam-reflinks-section
								#'org-roam-unlinked-references-section)
   org-roam-completion-everywhere t
   org-roam-dailies-capture-templates '(("d" "default" entry
										 "* %?"
										 :target (file+datetree "~/Dropbox/org/roam/daily/dailies.org" day)))
   org-roam-capture-templates'(("m" "main" plain "%?"
								:target (file+head "main/%<%Y%m%d%H%M%S>-${title}.org"
												   "#+TITLE: ${title}\n")
								:unnarrowed t)
							   ("b" "bibliography reference" plain
								(file "~/.emacs.d/private/orb-capture-template.org")
								:target (file+head "references/${citekey}.org" "#+title: ${title}\n")
								)
							   ("r" "reference" plain "%? \n %(v-i-or-nothing) \n\n%(v-a-or-nothing)"
								:target
								(file+head "references/%<%Y%m%d%H%M%S>-${title}.org" "#+title: ${title}\n")
								:unnarrowed t))
   org-roam-node-display-template (concat "${type:15} ${doom-hierarchy:120} ${backlinkscount:6}"
										  (propertize "${tags:*}" 'face 'org-tag))
   )

  (:after org
	(run-with-idle-timer 15 nil
						 #'org-roam-db-autosync-enable))

  (:hooks org-roam-buffer-postrender-functions (lambda ()
												 (org-latex-preview)
												 (org-display-inline-images)))
  (:when-loaded
	(add-to-list 'display-buffer-alist
				 '("\\*org-roam\\*"
		           (display-buffer-in-direction)
		           (direction . right)
		           (window-width . 0.4)
		           (window-height . fit-window-to-buffer))))
  (:after embark
	(defvar-keymap embark-org-roam-map
	  "i" #'org-roam-node-insert
	  "s" #'embark-collect
	  "b" #'eli/org-roam-backlinks-node-read)
	(add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-map))

	(defvar-keymap embark-org-heading-map
	  "i" #'embark-insert
	  "b" #'consult-org-heading-insert-backlink
	  "r" #'consult-org-headling-insert-reference
	  "w" #'embark-copy-as-kill
	  "q" #'embark-toggle-quit
	  "E" #'embark-export
	  "S" #'embark-collect
	  "L" #'embark-live
	  "B" #'embark-become
	  "A" #'embark-act-all
	  "C-s" #'embark-isearch
	  "SPC" #'mark
	  "DEL" #'delete-region)
	(add-to-list 'embark-keymap-alist
				 '(consult-org-heading . embark-org-heading-map))
	))

;;;; org-roam-ui
(setup org-roam-ui
  (:option*
   org-roam-ui-sync-theme t
   org-roam-ui-follow t
   org-roam-ui-update-on-save t
   org-roam-ui-open-on-start t))

;;;; org-mru-clock
(setup org-mru-clock
  (:option*
   org-mru-clock-capture-if-no-match '((".*" . "c"))
   org-mru-clock-how-many 50
   org-mru-clock-completing-read #'completing-read
   org-mru-clock-files #'org-agenda-files
   org-capture-templates-contexts
   '(("c" (org-mru-clock-capturing))
	 ;; TODO: make it work
     ;; ("1" (elemacs-global-interactive-capture))
     ;; ("2" (elemacs-global-interactive-capture))
	 )))

;;;; org-clock-convenience
(setup org-clock-convenience
  (:option*
   org-clock-convenience-clocked-agenda-re "^ +\\([^:]+\\):[[:space:]]*\\(\\([ 	012][0-9]\\):\\([0-5][0-9]\\)\\)-\\(\\([ 012]*[0-9]\\):\\([0-5][0-9]\\)\\|.*\\)?[[:space:]]+Clocked:[[:space:]]+\\(([0-9]+:[0-5][0-9])\\|(-)\\)")
  (:bind-into org-agenda-mode-map
	"M-<up>" org-clock-convenience-timestamp-up
	"M-<down>" org-clock-convenience-timestamp-down
	"<f9>" org-clock-convenience-fill-gap
	"<f10>" org-clock-convenience-fill-gap-both))

;;;; org-appear-mode
(setup org-appear-mode
  (:once (list :hooks 'org-mode-hook)
	(:once (list :hooks 'pre-command-hook)
	  (org-appear-mode))))

;;;; org-download
(setup org-download
  ;; (:iload org-download)
  (:option*
   org-download-method 'directory
   org-download-image-dir "~/Documents/org-images"
   org-download-heading-lvl nil
   org-download-delete-image-after-download t
   org-download-screenshot-method "flameshot gui --raw > %s"
   org-download-image-org-width 800
   org-download-annotate-function (lambda (_link) "")
   org-image-actual-width nil
   org-attach-method 'mv
   org-attach-auto-tag "attach"
   org-attach-store-link-p 't)
  (:hooks dired-mode-hook org-download-enable))

;;;; appt
(setup appt
  (:once (list :packages 'org-agenda)
	(require 'lib-appt))
  (:hooks org-agenda-finalize-hook eli/org-agenda-to-appt)
  (:after org-agenda
	(require 'appt)
	(require 'notifications)
    (run-with-idle-timer 10 nil
                         (lambda ()
                           (run-with-timer 0 3600 #'org-agenda-to-appt t))))
  (:option*
   appt-message-warning-time 30
   appt-display-interval 5
   appt-display-format 'window
   appt-disp-window-function #'appt-disp-window-and-notification))

;;;; org-media note
(setup org-media-note
  (:also-load org-link-edit
			  lib-org-media-note)
  (:advice
   org-media-note-play-online-video :after
   (lambda ()
	 (add-hook 'pre-command-hook #'eli/org-media-note-auto-pause nil t)))
  (:bind-into org-mode
	"s-[" eli/org-media-note-vedio-pause)
  (:option*
   org-media-note-screenshot-image-dir "~/Documents/org-images/"))

;;;; org-anki
(setup org-anki
  (:also-load lib-org-anki)
  (:option*
   org-fold-show-context-detail '((empty-headline . minimal)
								  (agenda . local)
								  (bookmark-jump . lineage)
								  (isearch . lineage)
								  (default . ancestors))
   org-anki-default-deck "Default"
   org-anki-default-match "+LEVEL=2"
   org-anki-model-fields '(("Basic" "Front" "Back")
                           ("Basic-Français" "Front" "Back")
                           ("Basic-English" "Front" "Back")
                           ("Basic (and reversed card)" "Front" "Back")
                           ("Basic (optional reversed card)" "Front" "Back")
                           ("NameDescr" "Name" "Descr")
                           ("Cloze" "Text"))
   org-anki-skip-function #'org-anki-skip
   )
  (:advice org-anki-sync-all :around eli/org-anki-around
		   org-anki-delete-all :around eli/org-anki-around)
  (:global
   "<f12>" #'org-anki-sync-region))

;;;; LaTeX
(setup org
  (:also-load ox-latex)
  (:option*
   org-highlight-latex-and-related nil
   ;; org-mode expanding "\ " as $\backslash$, so use "\ws" instead
   org-entities-user '(("ws" "\\ " nil " " " " " " " "))
   org-latex-prefer-user-labels t
   org-startup-with-latex-preview nil
   org-preview-latex-default-process 'dvisvgm
   org-preview-latex-process-alist'((dvisvgm :programs
											 ("xelatex" "dvisvgm")
											 :description "xdv > svg"
											 :message "you need to install the programs: xelatex and dvisvgm."
											 :use-xcolor t
											 :image-input-type "xdv"
											 :image-output-type "svg"
											 :image-size-adjust (1.7 . 1.5)
											 :latex-compiler
											 ("xelatex -no-pdf -interaction nonstopmode -shell-escape -output-directory %o %f")
											 :image-converter
											 ("dvisvgm %f -e -n -b min -c %S -o %O"))
									(imagemagick :programs
												 ("xelatex" "convert")
												 :description "pdf > png"
												 :message "you need to install the programs: xelatex and imagemagick."
												 :use-xcolor t
												 :image-input-type "pdf"
												 :image-output-type "png"
												 :image-size-adjust (1.0 . 1.0)
												 :latex-compiler
												 ("xelatex -interaction nonstopmode -output-directory %o %f")
												 :image-converter
												 ("convert -density %D -trim -antialias %f -quality 100 %O")))
   org-latex-hyperref-template "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c}, \n pdflang={%L},\n colorlinks=true,\n linkcolor=black}\n"
   org-format-latex-options '(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
										  ("begin" "$1" "$" "$$" "\\(" "\\["))
   org-latex-src-block-backend 'minted
   org-latex-minted-options '(("breaklines")
                              ("bgcolor" "bg"))
   org-latex-compiler "xelatex"
   org-latex-packages-alist '(("" "amsthm")
							  ("" "amsfonts")
							  ("" "tikz")
							  ("" "xcolor" t)
							  ("cache=false" "minted" t))
   org-latex-pdf-process '("xelatex -8bit --shell-escape -interaction nonstopmode -output-directory=%o %f"
						   "biber %b"
						   "xelatex -8bit --shell-escape -interaction nonstopmode -output-directory=%o %f"
						   "xelatex -8bit --shell-escape -interaction nonstopmode -output-directory=%o %f"
						   "rm -fr %b.out %b.log %b.tex %b.brf %b.bbl auto"
						   )
   org-latex-classes '(("Notes" "\\documentclass{ctexart}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n\\usepackage{/home/eli/.emacs.d/private/NotesTeXV3}"
						("\\part{%s}" . "\\part*{%s}")
						("\\section{%s}" . "\\section*{%s}")
						("\\subsection{%s}" . "\\subsection*{%s}")
						("\\subsubsection{%s}" . "\\subsubsection*{%s}")
						("\\paragraph{%s}" . "\\paragraph*{%s}")
						("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
					   ("article_cn" "\\documentclass[11pt]{ctexart}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n[EXTRA]\n\\definecolor{bg}{rgb}{0.95,0.95,0.95}"
						("\\section{%s}" . "\\section*{%s}")
						("\\subsection{%s}" . "\\subsection*{%s}")
						("\\subsubsection{%s}" . "\\subsubsection*{%s}")
						("\\paragraph{%s}" . "\\paragraph*{%s}")
						("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
					   ("beamer" "\\documentclass[ignorenonframetext,presentation]{beamer}"
						("\\section{%s}" . "\\section*{%s}")
						("\\subsection{%s}" . "\\subsection*{%s}"))
					   ("article" "\\documentclass[11pt]{article}"
						("\\section{%s}" . "\\section*{%s}")
						("\\subsection{%s}" . "\\subsection*{%s}")
						("\\subsubsection{%s}" . "\\subsubsection*{%s}")
						("\\paragraph{%s}" . "\\paragraph*{%s}")
						("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
					   ("report" "\\documentclass[11pt]{report}"
						("\\part{%s}" . "\\part*{%s}")
						("\\chapter{%s}" . "\\chapter*{%s}")
						("\\section{%s}" . "\\section*{%s}")
						("\\subsection{%s}" . "\\subsection*{%s}")
						("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
					   ("book" "\\documentclass[11pt]{book}"
						("\\part{%s}" . "\\part*{%s}")
						("\\chapter{%s}" . "\\chapter*{%s}")
						("\\section{%s}" . "\\section*{%s}")
						("\\subsection{%s}" . "\\subsection*{%s}")
						("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
  (:hook turn-on-org-cdlatex)
  (:advice
   org--make-preview-overlay :around eli/org-preview-show-label))

;;;; org simple ref
(setup org
  (:also-load lib-org-simple-ref))

;;;; org export
(setup ox
  (:also-load lib-ox)
  (:option*
   org-html-html5-fancy t
   org-html-doctype "html5"
   org-pandoc-options-for-docx '((standalone . nil))
   org-export-filter-paragraph-functions '(eli/strip-ws-maybe))
  (:advice
   org-export-get-ordinal :around eli/org-export-get-special-block-ordinal
   org-html-special-block :around eli/org-html-special-block-filter))

;;;; mixed-pitch
(setup mixed-pitch
  (:option*
   mixed-pitch-variable-pitch-cursor 'box
   mixed-pitch-set-height t)
  (:when-loaded
	(add-to-list 'mixed-pitch-fixed-pitch-faces 'org-date)
	(add-to-list 'mixed-pitch-fixed-pitch-faces 'org-tag)
	(add-to-list 'mixed-pitch-fixed-pitch-faces 'font-lock-comment-face))
  (:hooks org-mode-hook mixed-pitch-mode))

;;;; svg-tag-mode
(setup svg-tag-mode
  (:after org
	(require 'svg-tag-mode))
  (:also-load lib-svg-tag-mode)
  (:advice
   svg-tag-mode-on :around suppress-messages
   svg-tag-mode-off :around suppress-messages)
  (:option*
   svg-lib-style-default '(:background "#F5F5F5" :foreground "#37474f" :padding 0.5 :margin 0
									   :stroke 2 :radius 5 :alignment 0.5 :width 20 :height 0.9
									   :scale 0.75 :ascent center :crop-left nil :crop-right nil
									   :collection "material" :font-family "Cascadia Mono"
									   :font-size 11 :font-weight regular)
   svg-tag-action-at-point 'edit
   svg-lib-icon-collections '(("bootstrap" .
							   "https://icons.getbootstrap.com/assets/icons/%s.svg")
							  ("simple" .
							   "https://raw.githubusercontent.com/simple-icons/simple-icons/develop/icons/%s.svg")
							  ("material" .
							   "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
							  ("octicons" .
							   "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
							  ("boxicons" .
							   "https://boxicons.com/static/img/svg/regular/bx-%s.svg"))
   svg-tag-tags `(
				  ;; ;; Org tags
				  ;; (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
				  ;; (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

				  ;; Task priority
				  ("\\[#[A-Z]\\]" . ( (lambda (tag)
										(svg-tag-make tag :face 'org-priority 
													  :beg 2 :end -1 :margin 0
													  :height 1.1 :ascent 16))))

				  ;; Progress
				  ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
													  (svg-progress-percent
													   (substring tag 1 -2)))))
				  ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
													(svg-progress-count
													 (substring tag 1 -1)))))

				  ;; TODO / DONE
				  ("\\** \\(TODO\\)" . ((lambda (tag)
										  (svg-tag-make "TODO" :face 'org-todo
														:inverse t :margin 0
														:height 1.1 :ascent 16))))
				  ("\\** \\(NEXT\\)" . ((lambda (tag)
										  (svg-tag-make "NEXT" :face 'mindre-keyword
														:margin 0 :height 1.1
														:ascent 16))))
				  ("\\** \\(STARTED\\)" . ((lambda (tag)
											 (svg-tag-make "STARTED"
														   :face 'mindre-keyword
														   :inverse t :margin 0
														   :height 1.1 :ascent 16))))
				  ("\\** \\(SOMEDAY\\)" . ((lambda (tag)
											 (svg-tag-make "SOMEDAY"
														   :face 'mindre-keyword
														   :inverse t :margin 0
														   :height 1.1 :ascent 16))))
				  ("\\** \\(PROJECT\\)" . ((lambda (tag)
											 (svg-tag-make "PROJECT"
														   :face 'mindre-keyword
														   :inverse t :margin 0
														   :height 1.1 :ascent 16))))
				  ("\\** \\(DONE\\)" . ((lambda (tag)
										  (svg-tag-make "DONE" :face 'mindre-faded
														:inverse t :margin 0 :height 1.1
														:ascent 16))))
				  ;; ("FIXME" . ((lambda (tag)
				  ;;               (svg-tag-make "FIXME" :face 'mindre-critical :margin 0
				  ;;                             :height 1.1 :ascent 16))))


				  ;; Citation of the form [cite:@Knuth:1984] 
				  ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
													(svg-tag-make tag
																  :inverse t
																  :beg 7 :end -1
																  :crop-right t))))
				  ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
															 (svg-tag-make
															  tag
															  :end -1
															  :crop-left t))))

				  ;; Active date (with or without day name, with or without time)
				  (,(format "\\(<%s>\\)" date-re) .
				   ((lambda (tag)
					  (svg-tag-make tag :beg 1 :end -1 :margin 0 :ascent 14))))
				  (,(format "\\(<%s \\)%s>" date-re day-time-re) .
				   ((lambda (tag)
					  (svg-tag-make tag :beg 1 :inverse nil :crop-right t
									:margin 0 :ascent 14))))
				  (,(format "<%s \\(%s>\\)" date-re day-time-re) .
				   ((lambda (tag)
					  (svg-tag-make tag :end -1 :inverse t :crop-left t
									:margin 0 :ascent 14))))
				  ;; Inactive date  (with or without day name, with or without time)
				  (,(format "\\(\\[%s\\]\\)" date-re) .
				   ((lambda (tag)
					  (svg-tag-make tag :beg 1 :end -1 :margin 0
									:face 'org-date :ascent 14))))
				  (,(format "[^ ]\\{6\\}[^-]\\(\\[%s \\)%s\\][^-]" date-re day-time-re) .
				   ((lambda (tag)
					  (svg-tag-make tag :beg 1 :inverse nil
									:crop-right t :margin 0 :face 'org-date
									:ascent 14))))
				  (,(format "[^ ]\\{6\\}[^-]\\[%s \\(%s\\]\\)[^-]" date-re day-time-re) .
				   ((lambda (tag)
					  (svg-tag-make tag :end -1 :inverse t
									:crop-left t :margin 0 :face 'org-date
									:ascent 14))))))
  (:hooks org-mode-hook (lambda ()
                          (make-local-variable 'font-lock-extra-managed-props)
                          (svg-tag-mode))
		  org-agenda-finalize-hook eli/org-agenda-show-svg))


;;;; provide
(provide 'init-org)
;;; init-org.el ends here.
