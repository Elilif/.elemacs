;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-

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
(elemacs-require-package 'org-contrib)
(with-eval-after-load 'org
  
  ;; hide drawers
  (defun org-cycle-hide-drawers (state)
    "Re-hide all drawers after a visibility state change."
    (when (and (derived-mode-p 'org-mode)
	           (not (memq state '(overview folded contents))))
      (save-excursion
	    (let* ((globalp (memq state '(contents all)))
	           (beg (if globalp
			            (point-min)
                      (point)))
	           (end (if globalp
			            (point-max)
                      (if (eq state 'children)
			              (save-excursion
                            (outline-next-heading)
                            (point))
			            (org-end-of-subtree t)))))
          (goto-char beg)
          (while (re-search-forward org-drawer-regexp end t)
            (save-excursion
              (beginning-of-line 1)
              (when (looking-at org-drawer-regexp)
		        (let* ((start (1- (match-beginning 0)))
		               (limit
			            (save-excursion
                          (outline-next-heading)
                          (point)))
		               (msg (format
                             (concat
                              "org-cycle-hide-drawers:  "
                              "`:END:`"
                              " line missing at position %s")
                             (1+ start))))
                  (if (re-search-forward "^[ \t]*:END:" limit t)
                      (outline-flag-region start (point-at-eol) t)
                    (user-error msg))))))))))

  (defun eli/org-expand-all ()
    (interactive)
    (org-show-subtree)
    (org-unlogged-message "ALL")
    (setq org-cycle-subtree-status 'all))

  ;; a TODO entry automatically change to DONE when all children are done
  (defun eli/org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'eli/org-summary-todo)
  (setq org-use-fast-todo-selection 'expert)
  (setq org-log-into-drawer t)
  (setq org-startup-folded t)
  (setq org-hide-block-startup t)
  (setq org-hide-emphasis-markers t)
  (elemacs-require-package 'org-appear)

  ;;; org babel
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (shell . t)
                                   (C. t)))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (shell . t)
                                 (C . t)))

  (setq org-startup-with-inline-images t)
  ;; renumbering and sorting footnotes automatically after each deletion
  ;; or insertion
  (defun eli/clock-in-to-nest (kw)
    (if (org-get-todo-state)
	    "STARTED"))
  (setq org-footnote-auto-adjust t)
  (setq org-clock-in-switch-to-state `eli/clock-in-to-nest)
  (setq org-clock-mode-line-total 'today)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-continuously t)
  (setq org-clock-sound "~/.emacs.d/private/bellring.wav")
  ;; org todo keaywords
  (setq org-todo-keywords
	    (quote ((sequence "TODO(t/!)" "STARTED(s)" "|" "DONE(d!/!)")
		        (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
		        (sequence "WAITING(w@/!)" "NEXT(n!/!)"
                          "SOMEDAY(S)" "|" "CANCELLED(c@/!)"))))

  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "✎")
					                     ("#+END_SRC" . "□")
					                     ("#+begin_src" . "✎")
					                     ("#+end_src" . "□")
					                     ("[ ]" . "☐")
                                         ("[-]" . "🝕")
					                     ("[X]" . "🗹")
					                     ("#+begin_quote" . "»")
					                     ("#+end_quote" . "«")
					                     ("#+begin_verse" . "ζ")
					                     ("#+end_verse" . "ζ")
					                     ("#+begin_example" . "")
					                     ("#+end_example" . "")
                                         ("#+begin_export" . "🙑")
                                         ("#+end_export" . "🙔")
                                         ("#+END:" . "□")
                                         ("#+BEGIN:" . "✎")
                                         ("#+CAPTION:" . "🙛")
                                         ("#+ATTR_LATEX" . "🄛")
					                     ))
  (setq org-ellipsis "▼")
  )

(with-eval-after-load 'org
  (require 'org-protocol)
  (server-start))

;;; habits
(with-eval-after-load 'org
  (require 'org-habit)
  (setq org-habit-graph-column 1)
  (setq org-habit-preceding-days 10)
  (setq org-habit-following-days 2)
  (setq org-habit-show-habits-only-for-today nil)
  (defun org-habit-parse-todo (&optional pom)
    "Parse the TODO surrounding point for its habit-related data.
Returns a list with the following elements:

  0: Scheduled date for the habit (may be in the past)
  1: \".+\"-style repeater for the schedule, in days
  2: Optional deadline (nil if not present)
  3: If deadline, the repeater for the deadline, otherwise nil
  4: A list of all the past dates this todo was mark closed
  5: Repeater type as a string

This list represents a \"habit\" for the rest of this module."
    (save-excursion
      (if pom (goto-char pom))
      (cl-assert (org-is-habit-p (point)))
      (let* ((scheduled (org-get-scheduled-time (point)))
	         (scheduled-repeat (org-get-repeat (org-entry-get (point)
                                                              "SCHEDULED")))
	         (end (org-entry-end-position))
	         (habit-entry (org-no-properties (nth 4 (org-heading-components))))
	         closed-dates deadline dr-days sr-days sr-type)
	    (if scheduled
	        (setq scheduled (time-to-days scheduled))
	      (error "Habit %s has no scheduled date" habit-entry))
	    (unless scheduled-repeat
	      (error
	       "Habit `%s' has no scheduled repeat period or has an incorrect one"
	       habit-entry))
	    (setq sr-days (org-habit-duration-to-days scheduled-repeat)
	          sr-type (progn (string-match "[\\.+]?\\+" scheduled-repeat)
			                 (match-string-no-properties 0 scheduled-repeat)))
	    (unless (> sr-days 0)
	      (error "Habit %s scheduled repeat period is less than 1d" habit-entry))
	    (when (string-match "/\\([0-9]+[dwmy]\\)" scheduled-repeat)
	      (setq dr-days (org-habit-duration-to-days
			             (match-string-no-properties 1 scheduled-repeat)))
	      (if (<= dr-days sr-days)
	          (error "Habit %s deadline repeat period is less than
or equal to scheduled (%s)"
		             habit-entry scheduled-repeat))
	      (setq deadline (+ scheduled (- dr-days sr-days))))
	    (org-back-to-heading t)
	    (let* ((maxdays 99999)
	           (reversed org-log-states-order-reversed)
	           (search (if reversed 're-search-forward 're-search-backward))
	           (limit (if reversed end (point)))
	           (count 0)
	           (re (format
		            "^[ \t]*-[ \t]+\\(?:State \"%s\".*%s%s\\)"
		            (regexp-opt org-done-keywords)
		            org-ts-regexp-inactive
		            (let ((value (cdr (assq 'done org-log-note-headings))))
		              (if (not value) ""
			            (concat "\\|"
				                (org-replace-escapes
				                 (regexp-quote value)
				                 `(("%d" . ,org-ts-regexp-inactive)
				                   ("%D" . ,org-ts-regexp)
				                   ("%s" . "\"\\S-+\"")
				                   ("%S" . "\"\\S-+\"")
				                   ("%t" . ,org-ts-regexp-inactive)
				                   ("%T" . ,org-ts-regexp)
				                   ("%u" . ".*?")
				                   ("%U" . ".*?")))))))))
	      (unless reversed (goto-char end))
	      (while (and (< count maxdays) (funcall search re limit t))
	        (push (time-to-days
		           (org-time-string-to-time
		            (or (match-string-no-properties 1)
			            (match-string-no-properties 2))))
		          closed-dates)
	        (setq count (1+ count))))
	    (list scheduled sr-days deadline dr-days closed-dates sr-type))))

  (defun eli-habit-streaks (habit)
    (interactive)
    (let ((closed-days (nth 4 habit))
	      (counter 1)
	      (sum (length (nth 4 habit)))
	      (streaks 1)
	      (current-streaks 0)
	      (today (time-to-days (current-time)))
	      (max-streaks 1)
	      )
      (while (< counter (length closed-days))
	    (if (= (time-convert (time-subtract (nth  counter closed-days)
                                            (nth (1- counter) closed-days))
                             'integer) 1)
	        (progn (setq streaks (1+ streaks)))
	      (if (> streaks max-streaks)
	          (progn (setq max-streaks streaks)
		             (setq streaks 1)))
	      )
	    (setq counter (1+ counter)))
      (setq counter (1- counter))
      (if (= (time-convert (time-subtract today (nth counter closed-days))
                           'integer) 1)
	      (progn (setq current-streaks (1+ current-streaks))
		         (while (= (time-convert (time-subtract
                                          (nth  counter closed-days)
                                          (nth (1- counter) closed-days))
                                         'integer) 1)
		           (setq current-streaks (1+ current-streaks))
		           (setq counter (1- counter)))
		         )

	    )
      (if (> streaks max-streaks)
	      (setq max-streaks streaks))
      (insert (propertize (propertize (concat " ("
                                              (number-to-string current-streaks)
                                              "/"
                                              (number-to-string max-streaks)
                                              "/"
                                              (number-to-string sum) ")")
                                      'face 'mindre-faded)
                          'field t))))
  
  (defun org-habit-insert-consistency-graphs (&optional line)
    "Insert consistency graph for any habitual tasks."
    (let ((inhibit-read-only t)
	      (buffer-invisibility-spec '(org-link))
	      (moment (org-time-subtract nil
				                     (* 3600 org-extend-today-until))))
      (save-excursion
	    (goto-char (if line (point-at-bol) (point-min)))
	    (while (not (eobp))
	      (let ((habit (get-text-property (point) 'org-habit-p)))
	        (when habit
	          (move-to-column org-habit-graph-column t)
	          (delete-char (min (+ 1 org-habit-preceding-days
				                   org-habit-following-days)
				                (- (line-end-position) (point))))
	          (insert-before-markers
	           (org-habit-build-graph
		        habit
		        (time-subtract moment (days-to-time org-habit-preceding-days))
		        moment
		        (time-add moment (days-to-time org-habit-following-days))))
	          (end-of-line)
	          (eli-habit-streaks habit)
	          ))
	      (forward-line))))))

;;; hooks
(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'prettify-symbols-mode)
(add-hook 'org-mode-hook #'org-appear-mode)


;;; keybindings
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c c" #'org-capture)
(with-eval-after-load 'org
  (keymap-set org-mode-map "C-c C-j" nil)
  (keymap-set org-mode-map "<remap> <org-cycle-agenda-files>" #'avy-goto-char)
  (keymap-set org-mode-map "C-<tab>" #'eli/org-expand-all))



;;; agenda
(with-eval-after-load 'org
  (setq org-agenda-clock-consistency-checks
        '(:max-duration "10:00" :min-duration 0 :max-gap "0:00" :gap-ok-around
		                ("4:00")
		                :default-face
		                ((:background "DarkRed")
		                 (:foreground "white"))
		                :overlap-face nil :gap-face nil
                        :no-end-time-face nil :long-face nil :short-face nil))
  (setq org-agenda-clockreport-parameter-plist
        '(:link t :maxlevel 2 :fileskip0 t :sort
	            (3 . 84)
	            :formula %))
  (setq org-duration-format '((special . h:mm)))
  (setq org-agenda-span 'day)
  (setq org-agenda-show-inherited-tags nil)
  (setq org-agenda-window-setup 'only-window)
  (advice-add 'org-agenda-log-mode
              :after (lambda (&rest _arg) (beginning-of-buffer)))
  (advice-add 'org-agenda-redo-all
              :after (lambda (&rest _arg) (beginning-of-buffer)))
  (with-eval-after-load 'org
    (elemacs-require-package 'org-reverse-datetree))

  ;; custom org agenda view
  (setq org-agenda-log-mode-items '(clock))
  (setq org-agenda-log-mode-add-notes nil)

  (defun eli-make-progress (width)
    (let* ((today (time-to-day-in-year (current-time)))
           (percent (floor (* 100 (/ today 365.0))))
           (done (/ percent 100.0))
           (done-width (floor (* width done))))
      (concat
       "["
       (make-string done-width ?/)
       (make-string (- width done-width) ? )
       "]"
       (concat " " (number-to-string percent) "%"))))

  (setq org-agenda-custom-commands
	    '(("g" "GTD"
           ((agenda ""
                    ((org-agenda-overriding-header
                      (concat "Day-agenda (W"
                              (format-time-string "%U" (current-time))
                              ") "
                              (eli-make-progress 50)))))
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
		               ((org-agenda-overriding-header "Someday/Maybe")))
            ))))
  ;; change the progress color
  (defun eli-show-progress-color ()
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "\\([0-9]\\{2,3\\}\\)%" nil t)
        (let* ((percent (string-to-number (match-string 1)))
               percent-face)
          (cond
           ((< percent 33)
            (setq percent-face 'mindre-note))
           ((< percent 66)
            (setq percent-face 'mindre-keyword))
           ((< percent 90)
            (setq percent-face 'mindre-warning))
           ((< percent 100)
            (setq percent-face 'mindre-critical)))
          (overlay-put (make-overlay
                        (match-beginning 0) (match-end 0))
                       'face percent-face)))))
  (add-hook 'org-agenda-finalize-hook #'eli-show-progress-color)

  (defvar dynamic-agenda-files nil
    "dynamic generate agenda files list when changing org state")

  (defun update-dynamic-agenda-hook ()
    (let ((done (or (not org-state) ;; nil when no TODO list
                    (member org-state org-done-keywords)))
          (file (buffer-file-name))
          (agenda (funcall (ad-get-orig-definition 'org-agenda-files)) ))
      (unless (member file agenda)
	    (if done
            (save-excursion
              (goto-char (point-min))
              ;; Delete file from dynamic files
              ;; when all TODO entry changed to DONE
              (unless (search-forward-regexp org-not-done-heading-regexp nil t)
		        (customize-save-variable
		         'dynamic-agenda-files
		         (cl-delete-if (lambda (k) (string= k file))
			                   dynamic-agenda-files))))
          ;; Add this file to dynamic agenda files
          (unless (member file dynamic-agenda-files)
            (customize-save-variable 'dynamic-agenda-files
                                     (add-to-list 'dynamic-agenda-files
                                                  file)))))))

  (defun dynamic-agenda-files-advice (orig-val)
    (cl-union orig-val dynamic-agenda-files :test #'equal))

  (advice-add 'org-agenda-files :filter-return #'dynamic-agenda-files-advice)
  (with-eval-after-load 'org
    (add-to-list 'org-after-todo-state-change-hook
                 'update-dynamic-agenda-hook t)))


;;; capture
;; org capture
(with-eval-after-load 'org
  (setq org-agenda-dir "~/Dropbox/org")
  (setq org-directory "~/Dropbox/org")

  (setq org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir))
  (setq org-agenda-file-projects
        (expand-file-name "projects.org" org-agenda-dir))
  (setq org-agenda-file-habit (expand-file-name "daily.org" org-agenda-dir))
  (setq org-agenda-file-notes (expand-file-name "notes.org" org-agenda-dir))
  (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
  (setq org-agenda-file-te (expand-file-name "words.org" org-agenda-dir))
  (setq org-agenda-file-lists (expand-file-name "lists.org" org-agenda-dir))
  (setq org-agenda-files '("~/Dropbox/org/journal.org"
                           "/home/eli/Dropbox/org/古文.org"
                           "/home/eli/Dropbox/org/Français.org"
                           "/home/eli/Dropbox/org/daily.org"
                           "/home/eli/Dropbox/org/lists.org"
                           "/home/eli/Dropbox/org/inbox.org"
                           "/home/eli/Dropbox/org/words.org"
                           "/home/eli/Dropbox/org/projects.org"))
  (defun eli/capture-report-date-file ()
    (let ((name (read-string "Name: ")))
      (expand-file-name (format "%s-%s.org"
				                (format-time-string "%Y-%m-%d")
				                name) "~/Dropbox/org/blog")))

  ;; add a property to create id
  (defun eli/org-capture-maybe-create-id ()
    (when (org-capture-get :create-id)
      (org-id-get-create)))
  (add-hook 'org-capture-prepare-finalize-hook #'eli/org-capture-maybe-create-id)

  ;; from: https://stackoverflow.com/questions/21073859/is-there-a-way-
  ;; with-org-capture-templates-to-not-insert-a-line-if-initial-conten
  (defun v-i-or-nothing ()
    (let ((v-i (plist-get org-store-link-plist :initial)))
      (if (equal v-i "")
          ""
	    (concat "\n#+begin_quote\n" v-i "\n#+end_quote\n"))))

  (defun v-a-or-nothing ()
    (let ((v-a (plist-get org-store-link-plist :annotation)))
      (if (equal v-a "")
          ""
	    (concat "- reference :: " v-a))))

  (defun v-i-or-nothing-word ()
    (let* ((v-i (plist-get org-store-link-plist :initial))
           (new-string (string-clean-whitespace
                        (replace-regexp-in-string "\n" " " v-i))))
      new-string))

  ;; better fill region in capture
  (defun eli/fill-region ()
    (save-excursion
      (push-mark)
      (push-mark (point-max) nil t)
      (goto-char (minibuffer-prompt-end))
      (org-fill-paragraph nil t)))
  (add-hook 'org-capture-prepare-finalize-hook 'eli/fill-region)

  (defun eli-org-capture-template-goto-today (format-string start end point)
    "Set point for capturing at what capture target file+headline
with headline set to %l would do."
    (org-capture-put :target (list 'file+headline
                                   (nth 1 (org-capture-get :target))
                                   (format-time-string format-string)))
    (org-capture-put-target-region-and-position)
    (widen)
    (let ((hd (nth 2 (org-capture-get :target))))
      (goto-char (point-min))
      (if (re-search-forward
	       (format org-complex-heading-regexp-format
                   (regexp-quote (substring hd start end)))
	       nil t)
	      (goto-char (point-at-bol))
	    (goto-char point)
	    (insert "\n")
	    (insert "* " hd "\n")
	    (beginning-of-line 0))))

  (setq org-capture-templates
	    '(
          ("t" "Todo" entry (file org-agenda-file-inbox)
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
                                 (eli-org-capture-template-goto-today
                                  "%Y-%m-%d" 0 10 361)))
	       "* %?"
	       )
	      ("B" "Blogs" plain (file eli/capture-report-date-file)
	       "#+TITLE: %?\n#+DATE: %<%Y-%m-%d>\n#+STARTUP: showall\
\n#+OPTIONS: toc:nil H:2 num:2\n"
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
                                   (eli-org-capture-template-goto-today
                                    "TODO %Y-%m-%d [/]" 5 15 81)))
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
  )

;;; time report
;; from: https://emacs.stackexchange.com/questions/31683
;; /schedule-org-task-for-last-day-of-every-month
;; ORG-MODE:  * My Task
;;              SCHEDULED: <%%(diary-last-day-of-month date)>
;; DIARY:  %%(diary-last-day-of-month date) Last Day of the Month
;; See also:  (setq org-agenda-include-diary t)
;; (diary-last-day-of-month '(2 28 2017))
(defun diary-last-day-of-month (date)
  "Return `t` if DATE is the last day of the month."
  (let* ((day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-day-of-month
          (calendar-last-day-of-month month year)))
    (= day last-day-of-month)))


;;; reminding
(with-eval-after-load 'org
  ;; 更新agenda时，同步appt
  (defun eli/org-agenda-to-appt ()
    "call org-agenda-to-appt with refresh."
    (org-agenda-to-appt t))
  (add-hook 'org-agenda-finalize-hook #'eli/org-agenda-to-appt)

  (with-eval-after-load 'org-agenda
    ;; 每小时同步一次appt,并且现在就开始同步
    (run-at-time 30 3600 'org-agenda-to-appt t)
    ;; 提前半小时提醒
    (setq appt-message-warning-time 30)
    (setq appt-display-interval 5)
    (require 'notifications)
    (setq appt-display-format 'window)
    (setq appt-disp-window-function #'appt-disp-window-and-notification))

  (defun appt-disp-window-and-notification (min-to-appt current-time appt-msg)
    (if (atom min-to-appt)
	    (notifications-notify :timeout (* appt-display-interval 60000)
                                        ;; 一直持续到下一次提醒
			                  :title (format "%s分钟内有新的任务" min-to-appt)
			                  :body appt-msg)
      (dolist (i (number-sequence 0 (1- (length min-to-appt))))
	    (notifications-notify :timeout (* appt-display-interval 60000)
                                        ;; 一直持续到下一次提醒
			                  :title (format "%s分钟内有新的任务"
                                             (nth i min-to-appt))
			                  :body (nth i appt-msg)))))
  )

;;; org-refile
(with-eval-after-load 'org
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets
	    '((nil :maxlevel . 5)
          (org-agenda-files :maxlevel . 5))))

;;; rime
(elemacs-require-package 'rime)
(with-eval-after-load 'org
  (defun +rime-predicate-punctuation-line-begin-p ()
    "Enter half-width punctuation at the beginning of the line.
  Detect whether the current cursor is at the beginning of a
  line and the character last inputted is symbol.
  Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
    (<= (point) (save-excursion (back-to-indentation) (point))))

  (defun rime-predicate-after-latin-char-p ()
    "If the cursor is after a latin character.
Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
    (and (> (point) (save-excursion (back-to-indentation) (point)))
	     (let ((string (buffer-substring (point) (max (line-beginning-position)
                                                      (- (point) 80)))))
           (and (string-match-p "\\cl$" string)
                (not (string-match-p " $" string))))))

  (setq eli/prefer-English t)
  (defun eli/input-switch ()
    (interactive)
    (if (not eli/prefer-English)
	    (progn
          (add-to-list 'rime-disable-predicates
                       'rime-predicate-space-after-ascii-p)
		  (add-to-list 'rime-disable-predicates
                       '+rime-predicate-punctuation-line-begin-p)
	      (setq eli/prefer-English t))
      (progn
	    (setq rime-disable-predicates
	          (seq-difference rime-disable-predicates
                              '(rime-predicate-space-after-ascii-p
							    +rime-predicate-punctuation-line-begin-p)))
	    (setq eli/prefer-English nil)
	    )))

  (defun +rime-convert-string-at-point (&optional return-cregexp)
    "将光标前的字符串转换为中文."
    (interactive "P")
    (rime-force-enable)
    (let ((string (if mark-active
                      (buffer-substring-no-properties
		               (region-beginning) (region-end))
                    (buffer-substring-no-properties
                     (point) (max (line-beginning-position) (- (point) 80)))))
          code
          length)
      (cond ((string-match "\\([a-z'-]+\\|[[:punct:]]\\) *$" string)
             (setq code (replace-regexp-in-string
			             "^[-']" ""
			             (match-string 0 string)))
             (setq length (length code))
             (setq code (replace-regexp-in-string " +" "" code))
             (if mark-active
		         (delete-region (region-beginning) (region-end))
	           (when (> length 0)
		         (delete-char (- 0 length))))
             (when (> length 0)
	           (setq unread-command-events
                     (append (listify-key-sequence code)
                             unread-command-events))))
            (t (message "`+rime-convert-string-at-point' did nothing.")))))

  (setq default-input-method "rime"
	    rime-user-data-dir "~/.emacs.d/rime"
	    rime-disable-predicates '(rime-predicate-prog-in-code-p
				                  rime-predicate-space-after-ascii-p
				                  rime-predicate-after-ascii-char-p
				                  +rime-predicate-punctuation-line-begin-p
				                  rime-predicate-org-in-src-block-p
				                  rime-predicate-space-after-cc-p
				                  rime-predicate-current-uppercase-letter-p
				                  rime-predicate-hydra-p
				                  rime-predicate-after-latin-char-p
				                  )
	    rime-show-candidate 'nil
	    rime-inline-ascii-trigger 'shift-l
        rime-deactivate-when-exit-minibuffer nil)
  (keymap-global-set "C-s-k" #'rime-inline-ascii)
  (keymap-global-set "C-s-j" #'+rime-convert-string-at-point)
  (add-hook 'org-mode-hook 'toggle-input-method)
  )

(elemacs-require-package 'org-superstar)
(setq org-superstar-headline-bullets-list '("⦿" "⊚" "𐰧" "◯" "●" "►" "▻")
      org-superstar-prettify-item-bullets nil)
(add-hook 'org-mode-hook #'org-superstar-mode)


;;; roam
(elemacs-require-package 'org-roam)
(with-eval-after-load 'org-roam
  (setq org-roam-directory "~/Dropbox/org/roam/")
  (setq org-roam-db-gc-threshold most-positive-fixnum
	    org-id-link-to-org-use-id 'create-if-interactive)

  ;; Preview LaTeX & images in Org Roam window
  (add-hook 'org-roam-buffer-postrender-functions
            (lambda ()
              (org-latex-preview)
              (org-display-inline-images)))

  (add-to-list 'display-buffer-alist
	           '("\\*org-roam\\*"
		         (display-buffer-in-direction)
		         (direction . right)
		         (window-width . 0.4)
		         (window-height . fit-window-to-buffer)))
  (setq org-roam-mode-section-functions
	    (list #'org-roam-backlinks-section
	          #'org-roam-reflinks-section
	          #'org-roam-unlinked-references-section
	          ))
  (defun org-roam-unlinked-references-section (node)
    "The unlinked references section for NODE.
References from FILE are excluded."
    (when (and (executable-find "rg")
               (org-roam-node-title node)
               (not (string-match "PCRE2 is not available"
                                  (shell-command-to-string
                                   "rg --pcre2-version"))))
      (let* ((titles (cons (org-roam-node-title node)
                           (org-roam-node-aliases node)))
             (rg-command (concat "rg -L -o --vimgrep -P -i "
                                 (mapconcat (lambda (glob) (concat "-g " glob))
                                            (org-roam--list-files-search-globs
                                             org-roam-file-extensions)
                                            " ")
                                 (format " '\\[([^[]]++|(?R))*\\]%s' "
                                         (mapconcat
                                          (lambda (title)
                                            (setq eli-test title)
                                            (format "|(\\b%s\\b)"
                                                    (shell-quote-argument
                                                     title)))
                                          titles ""))
                                 org-roam-directory))
             (results (split-string (shell-command-to-string rg-command) "\n"))
             f row col match)
        (magit-insert-section (unlinked-references)
          (magit-insert-heading "Unlinked References:")
          (dolist (line results)
            (save-match-data
              (when (string-match org-roam-unlinked-references-result-re line)
                (setq f (match-string 1 line)
                      row (string-to-number (match-string 2 line))
                      col (string-to-number (match-string 3 line))
                      match (match-string 4 line))
                (when (and match
                           (not (file-equal-p (org-roam-node-file node) f))
                           (member (downcase match) (mapcar #'downcase titles)))
                  (magit-insert-section section (org-roam-grep-section)
                    (oset section file f)
                    (oset section row row)
                    (oset section col col)
                    (insert (propertize (format "%s:%s:%s"
                                                (truncate-string-to-width
                                                 (file-name-base f) 15 nil nil t)
                                                row col)
                                        'font-lock-face 'org-roam-dim)
                            " "
                            (org-roam-fontify-like-in-org-mode
                             (org-roam-unlinked-references-preview-line f row))
                            "\n"))))))
          (insert ?\n)))))
  (setq org-roam-completion-everywhere t)

  (setq org-roam-dailies-capture-templates
	    '(("d" "default" entry
           "* %?"
           :if-new (file+datetree "~/Dropbox/org/roam/daily/dailies.org" day))))
  (setq org-roam-capture-templates
        '(("m" "main" plain "%?"
           :if-new (file+head "main/%<%Y%m%d%H%M%S>.org"
							  "#+TITLE: ${title}\n")
           :unnarrowed t)
		  ("b" "bibliography reference" plain
		   (file "~/.emacs.d/private/orb-capture-template.org")
		   :if-new (file+head "references/${citekey}.org" "#+title: ${title}\n")
		   )
		  ("r" "reference" plain "%? \n %(v-i-or-nothing) \n\n%(v-a-or-nothing)"
		   :if-new
		   (file+head "references/%<%Y%m%d%H%M%S>.org" "#+title: ${title}\n")
		   :unnarrowed t)))
  (run-at-time 20 nil
	           #'org-roam-setup)
  
  (with-eval-after-load 'org-roam
    ;; Codes blow are used to general a hierachy
    ;; for title nodes that under a file
    (cl-defmethod org-roam-node-doom-filetitle ((node org-roam-node))
      "Return the value of \"#+title:\" (if any) from file that NODE resides in.
      If there's no file-level title in the file, return empty string."
      (or (if (= (org-roam-node-level node) 0)
              (org-roam-node-title node)
            (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
	      ""))
    (cl-defmethod org-roam-node-doom-hierarchy ((node org-roam-node))
      "Return hierarchy for NODE, constructed of its file title, OLP and
direct title.
        If some elements are missing, they will be stripped out."
      (let ((title     (org-roam-node-title node))
            (olp       (org-roam-node-olp   node))
            (level     (org-roam-node-level node))
            (filetitle (org-roam-node-doom-filetitle node))
            (separator (propertize " > " 'face 'shadow)))
	    (cl-case level
	      ;; node is a top-level file
	      (0 filetitle)
	      ;; node is a level 1 heading
	      (1 (concat (propertize filetitle 'face '(shadow italic))
                     separator title))
	      ;; node is a heading with an arbitrary outline path
	      (t (concat (propertize filetitle 'face '(shadow italic))
                     separator (propertize (string-join olp " > ")
                                           'face '(shadow italic))
                     separator title)))))

    (cl-defmethod org-roam-node-type ((node org-roam-node))
      "Return the TYPE of NODE."
      (condition-case nil
	      (file-name-nondirectory
	       (directory-file-name
            (file-name-directory
             (file-relative-name (org-roam-node-file node) org-roam-directory))))
	    (error "")))

    (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
      "Access slot \"backlinks\" of org-roam-node struct CL-X"
      (let* ((count (caar (org-roam-db-query
			               [:select (funcall count source)
				                    :from links
				                    :where (= dest $s1)
				                    :and (= type "id")]
			               (org-roam-node-id node)))))
	    (format "[%d]" count)))
    
    (cl-defmethod org-roam-node-backlinkscount-number ((node org-roam-node))
      "Access slot \"backlinks\" of org-roam-node struct CL-X. This
     is identical to `org-roam-node-backlinkscount' with the
     difference that it returns a number instead of a fromatted
     string. This is to be used in
     `eli-org-roam-node-sort-by-backlinks'"
      (let* ((count (caar (org-roam-db-query
			               [:select (funcall count source)
				                    :from links
				                    :where (= dest $s1)
				                    :and (= type "id")]
			               (org-roam-node-id node)))))
	    count))

    (setq org-roam-node-display-template
          (concat "${type:15} ${doom-hierarchy:120} ${backlinkscount:6}"
                  (propertize "${tags:*}" 'face 'org-tag))))
  
  (defun eli-org-roam-node-sort-by-backlinks (completion-a completion-b)
    "Sorting function for org-roam that sorts the list of nodes by
   the number of backlinks. This is the sorting function in
   `eli-org-roam-backlinks--read-node-backlinks'"
    (let ((node-a (cdr completion-a))
	      (node-b (cdr completion-b)))
	  (>= (org-roam-node-backlinkscount-number node-a)
	      (org-roam-node-backlinkscount-number node-b))))

  ;; embark support
  ;; from https://github.com/Vidianos-Giannitsis/Dotfiles/blob/master/emacs/
  ;; .emacs.d/libs/zettelkasten.org
  (defun eli-org-roam-backlinks-query* (NODE)
    "Gets the backlinks of NODE with `org-roam-db-query'."
    (org-roam-db-query
     [:select [source dest]
		      :from links
		      :where (= dest $s1)
		      :and (= type "id")]
     (org-roam-node-id NODE)))

  (defun eli-org-roam-backlinks-p (SOURCE NODE)
    "Predicate function that checks if NODE is a backlink of SOURCE."
    (let* ((source-id (org-roam-node-id SOURCE))
	       (backlinks (eli-org-roam-backlinks-query* SOURCE))
	       (id (org-roam-node-id NODE))
	       (id-list (list id source-id)))
      (member id-list backlinks)))

  (defun eli-org-roam-backlinks--read-node-backlinks (source)
    "Runs `org-roam-node-read' on the backlinks of SOURCE.
 The predicate used as `org-roam-node-read''s filter-fn is
 `eli-org-roam-backlinks-p'."
    (org-roam-node-read nil (apply-partially #'eli-org-roam-backlinks-p source)
                        #'eli-org-roam-node-sort-by-backlinks))

  (defun eli-org-roam-backlinks-node-read (entry)
    "Read a NODE and run `eli-org-roam-backlinks--read-node-backlinks'."
    (let* ((node (get-text-property 0 'node entry))
           (backlink (eli-org-roam-backlinks--read-node-backlinks node)))
      (find-file (org-roam-node-file backlink))))

  (embark-define-keymap embark-org-roam-map
    "Keymap for Embark org roam actions."
    ("i" org-roam-node-insert)
    ("s" embark-collect)
    ("b" eli-org-roam-backlinks-node-read))
  
  (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-map))

  (defun consult-heading-insert-backlink (target)
    (let* ((marker (plist-get
                    (text-properties-at 0 target)
                    'consult--candidate))
           (headline-name (substring (org-no-properties target)
                                     0 -1))
           (headline-id (save-excursion
                          (with-current-buffer
                              (marker-buffer marker)
                            (goto-char marker)
                            (org-id-get-create)))))
      (org-insert-link
	   nil (concat "id:" headline-id) headline-name)))

  (embark-define-keymap embark-org-heading-map
    "Keymap for Embark heading actions."
    ("i" embark-insert)
    ("b" consult-heading-insert-backlink)
    ("w" embark-copy-as-kill)
    ("q" embark-toggle-quit)
    ("E" embark-export)
    ("S" embark-collect)
    ("L" embark-live)
    ("B" embark-become)
    ("A" embark-act-all)
    ("C-s" embark-isearch)
    ("SPC" mark)
    ("DEL" delete-region))

  (add-to-list 'embark-keymap-alist '(consult-org-heading . embark-org-heading-map))

  (elemacs-require-package 'org-roam-ui)
  (setq org-roam-ui-sync-theme t
	    org-roam-ui-follow t
	    org-roam-ui-update-on-save t
	    org-roam-ui-open-on-start t)
  )

;; clock
(elemacs-require-package 'org-mru-clock)
(with-eval-after-load 'embark
  (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook))
(with-eval-after-load 'org
  (require 'org-mru-clock)
  (defun org-mru-clock-goto (task)
    "Go to buffer and position of the TASK (cons of description and marker)."
    (interactive (progn
                   (org-mru-clock-to-history)
                   (list (org-mru-clock--completing-read))))
    (let ((m (cdr task)))
      (switch-to-buffer (org-base-buffer (marker-buffer m)))
      (if (or (< m (point-min)) (> m (point-max))) (widen))
      (goto-char m)
      (org-show-entry)
      (org-back-to-heading t)
      (org-cycle-hide-drawers 'children)
      (org-reveal)))
  (setq org-mru-clock-capture-if-no-match '((".*" . "c"))
	    org-mru-clock-how-many 50
        org-mru-clock-completing-read #'completing-read
	    org-mru-clock-files #'org-agenda-files
	    org-capture-templates-contexts
        '(("c" (org-mru-clock-capturing))
          ("1" (elemacs-global-interactive-capture))
          ("2" (elemacs-global-interactive-capture)))) )

(elemacs-require-package 'org-clock-convenience)
(setq org-clock-convenience-clocked-agenda-re "^ +\\([^:]+\\):[[:space:]]*\\(\\([ 	012][0-9]\\):\\([0-5][0-9]\\)\\)-\\(\\([ 012]*[0-9]\\):\\([0-5][0-9]\\)\\|.*\\)?[[:space:]]+Clocked:[[:space:]]+\\(([0-9]+:[0-5][0-9])\\|(-)\\)")
(with-eval-after-load 'org-agenda
  (keymap-set org-agenda-mode-map "M-<up>"
              #'org-clock-convenience-timestamp-up)
  (keymap-set org-agenda-mode-map "M-<down>"
              #'org-clock-convenience-timestamp-down)
  (keymap-set org-agenda-mode-map "<f9>"
              #'org-clock-convenience-fill-gap)
  (keymap-set org-agenda-mode-map "<f10>"
              #'org-clock-convenience-fill-gap-both))

;; fix M-j
(defun eli-org-fill-prefix ()
  "Set `fill-prefix' to the empty string."
  (setq fill-prefix ""))
(add-hook 'org-mode-hook #'eli-org-fill-prefix)

(with-eval-after-load 'org
  ;;; org-download
  (elemacs-require-package 'org-download)
  (require 'org-download)
  (setq-default org-download-method 'directory
		        org-download-image-dir "~/Documents/org-images"
		        org-download-heading-lvl nil
		        org-download-delete-image-after-download t
		        org-download-screenshot-method "flameshot gui --raw > %s"
		        org-download-image-org-width 800
		        org-download-annotate-function (lambda (link) "")
                ;; Don't annotate
		        )

  ;; org-download use buffer-local variables. Set it individually in files.
  ;; Otherwise, put things flatly in misc folder.

  (add-hook 'dired-mode-hook 'org-download-enable)
  (global-set-key (kbd "C-c l") 'org-store-link)
  ;; crop in X11 first, and paste within here later
  ;; Use #+ATTR_ORG: :width 300px to customized image display width
  (setq org-image-actual-width nil)
  ;; org-attach method
  (setq-default org-attach-method 'mv
		        org-attach-auto-tag "attach"
		        org-attach-store-link-p 't)
  )

;;; misc
(with-eval-after-load 'org
  (setq org-indirect-buffer-display 'current-window)
  (setq org-export-preserve-breaks t)
  (setq org-adapt-indentation t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame)))

  ;; remove superfluous whitespace.
  (defun eli-org-fill-paragraph (&optional arg)
    (when arg
      (fill-paragraph)
      (goto-char (mark))))

  (defun eli-org-clean-sentence (&optional arg)
    (when arg
      (let* ((temp (pop kill-ring))
             (new-string (string-clean-whitespace
                          (replace-regexp-in-string "\n" " " temp))))
        (push new-string kill-ring))))

  (defun eli-unfill-string (string)
    (if (and (memq major-mode '(org-mode mu4e-view-mode
                                         elfeed-show-mode nov-mode))
             (member (prefix-numeric-value current-prefix-arg) '(4 16 64)))
        (string-clean-whitespace
         (replace-regexp-in-string "\n" " " string))
      string))
  (advice-add 'filter-buffer-substring :filter-return #'eli-unfill-string)
  (advice-add 'org-yank :before #'eli-org-clean-sentence)
  (advice-add 'org-yank :after #'eli-org-fill-paragraph)

  ;; movie rating
  (defun eli/get-tag-counts ()
    (interactive)
    (let ((all-tags '()))
      (org-map-entries
       (lambda ()
	     (let ((tag-string (car (last (org-heading-components)))))
	       (when tag-string
	         (setq all-tags
		           (append all-tags (split-string tag-string ":" t))))))
       "+LEVEL=1")
      (list (completing-read "Select a tag:" all-tags))))
  (defun eli/entry-rating ()
    (interactive)
    (let* ((eli/temp)
	       (eli/rate))
      (setq eli/temp (org-map-entries (lambda ()
                                        (string-to-number
                                         (if (org-entry-get nil "Rating")
                                             (org-entry-get nil "Rating")
                                           "0")))
                                      "+Rating>=0" `tree))
      (pop eli/temp)
      (setq eli/rate (if (= (length eli/temp) 0)
                         0
                       (/ (apply `+  eli/temp) (length eli/temp))))
      (org-set-property "Rating" (format "%.2f" eli/rate))))
  (defun eli/rating (type)
    (interactive (eli/get-tag-counts))
    (org-map-entries 'eli/entry-rating
                     (concat type "+LEVEL=2-TODO=\"DONE\"-TODO=\"CANCELLED\"")))
  (define-key org-mode-map (kbd "<f8>") 'eli/rating)
  (define-key org-mode-map (kbd "<f7>") 'eli/entry-rating)


  ;; rating films
  (defun eli/get-film-rating ()
    (interactive)
    (let ((ratings)
	      (dimensions
           (list "剧情" "演技" "美术" "声效" "画面"
                 "剪辑" "运镜" "立意" "人物" "细节")))
      (cl-loop for dim in dimensions
	           do
	           (push (string-to-number (org-entry-get (point) dim)) ratings))
      (org-entry-put (point) "Rating" (format "%.2f" (/ (-sum ratings) 10.0)))))

  (defun eli/set-film-ratings ()
    (interactive)
    (let ((dimensions (list "剧情" "演技" "美术" "声效" "画面"
                            "剪辑" "运镜" "立意" "人物" "细节")))
      (cl-loop for dim in dimensions
	           do
	           (org-entry-put (point)
                              dim
                              (read-from-minibuffer
                               (format "Set rating for %s : " dim))))))
  (define-key org-mode-map (kbd "<f9>") 'eli/set-film-ratings)
  (define-key org-mode-map (kbd "<f10>") 'eli/get-film-rating)

  ;; Learn from: https://xenodium.com/emacs-dwim-do-what-i-mean/
  (defun ar/org-insert-link-dwim ()
    "Like `org-insert-link' but with personal dwim preferences."
    (interactive)
    (let* ((point-in-link (org-in-regexp org-link-any-re 1))
           (clipboard-url (when (string-match-p "^http" (current-kill 0))
                            (current-kill 0)))
           (region-content (when (region-active-p)
                             (buffer-substring-no-properties (region-beginning)
                                                             (region-end)))))
      (cond ((and region-content clipboard-url (not point-in-link))
             (delete-region (region-beginning) (region-end))
             (insert (org-make-link-string clipboard-url region-content)))
            ((and clipboard-url (not point-in-link))
             (insert (org-make-link-string
                      clipboard-url
                      (read-string "title: "
                                   (with-current-buffer
                                       (url-retrieve-synchronously clipboard-url)
                                     (dom-text (car
                                                (dom-by-tag
                                                 (libxml-parse-html-region
                                                  (point-min)
                                                  (point-max))
                                                 'title))))))))
            (t
             (call-interactively 'org-insert-link)))))
  (keymap-set org-mode-map "C-c C-l" #'ar/org-insert-link-dwim)
  )

;;; anki integration
(elemacs-require-package 'htmlize)
(elemacs-require-package 'org-anki)
(with-eval-after-load 'org
  (require 'org-anki)
  (setq org-anki-default-deck "Default")
  (defun org-anki-skip ()
    "Skip headlines with \"noanki\" property.
Used by `org-anki-skip-function'"
    (if (string= "t" (org-entry-get nil "NOANKI"))
        (point)))
  (setq org-anki-skip-function #'org-anki-skip)

  (defun eli-org-anki-sync-item (item)
    (org-anki-connect-request
     (org-anki--create-note-single item)
     (lambda (the-result)
       (message
        "org-anki: note succesfully updated: %s"
        the-result))
     (lambda (the-error)
       (org-anki--report-error
        "Couldn't update note, received: %s"
        the-error))))
  
  (defmacro eli-org-anki-install (fun-name reg front &optional back)
  `(defun ,(intern (format "org-anki-sync-%s" fun-name)) ()
     (interactive)
     (save-excursion
       (save-restriction
         (org-back-to-heading)
         (org-narrow-to-subtree)
         (while (re-search-forward ,reg nil t)
           (let*
               ((front-string (match-string-no-properties ,front))
                (back-string (if ,back
                                 (match-string-no-properties ,back)
                               nil))
                (front (org-anki--string-to-html (string-clean-whitespace
                                                  front-string)))
                (maybe-id (org-entry-get nil org-anki-prop-note-id))
                (back (if ,back
                          (org-anki--back-post-processing
                           (org-anki--string-to-html
                            (string-clean-whitespace
                             back-string)))
                        ""))
                (tags (org-anki--get-tags))
                (deck (save-excursion
                        (save-restriction
                          (widen)
                          (org-anki--find-prop
                           org-anki-prop-deck org-anki-default-deck))))
                (type (org-anki--find-prop
                       org-anki-note-type org-anki-default-note-type))
                (note-start (point))
                (card (make-org-anki--note
                       :maybe-id (if (stringp maybe-id)
                                     (string-to-number maybe-id))
                       :front    front
                       :back     back
                       :tags     tags
                       :deck     deck
                       :type     type
                       :point    note-start)))
             (eli-org-anki-sync-item card)))))))
  (eli-org-anki-install "description" (rx bol
                                          (* " ")
                                          "- "
                                          (group (* any))
                                          " :: "
                                          (group (* any)
                                                 (? "\n")
                                                 (* (** 1 2 " ")
                                                    (* any)
                                                    (? "\n")))) 1 2)
  (eli-org-anki-install "checkbox" "^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\| \\|\\([0-9]+\\)/\\1\\)\\] \\(.*\n?\\(?: \\{1,2\\}.*\n?\\)*\\)" 2)
  
  (defun org-anki-sync-region (beg end)
    (interactive "r")
    (let* ((text (buffer-substring beg end))
           (regexp (read-regexp "Input a Regexp: "))
           (void (string-match regexp
                               text))
           (front-string (match-string-no-properties 1 text))
           (back-string (match-string-no-properties 2 text))
           (front (org-anki--string-to-html
                   (read-string "Front Card: "
                                (string-clean-whitespace front-string))))
           (back (org-anki--back-post-processing
                  (org-anki--string-to-html
                   (read-string "Back Card: "
                                (if (member (prefix-numeric-value
                                             current-prefix-arg)
                                            '(4 16 64))
                                    (string-clean-whitespace back-string)
                                  back-string)))))
           (deck (read-string "Input Deck to import: "))
           (type (org-anki--find-prop
                  org-anki-note-type org-anki-default-note-type))
           (note-start (point))
           (maybe-id (org-entry-get nil org-anki-prop-note-id))
           (tags (org-anki--get-tags))
           (card (make-org-anki--note
                  :maybe-id (if (stringp maybe-id)
                                (string-to-number maybe-id))
                  :front    front
                  :back     back
                  :tags     tags
                  :deck     deck
                  :type     type
                  :point    note-start)))
      (eli-org-anki-sync-item card)
      (deactivate-mark)))

  (keymap-global-set "<f12>" #'org-anki-sync-region)
  
  (defalias 'org-anki-sync-word
    (kmacro "C-u <f12> C-s-k \\(.*\\)：\\(\\(?:.* C-q C-j ?\\)*\\) <return> SPC 的语境 <return> <return> Words <return>"))

  (defalias 'org-anki-sync-poem
    (kmacro "<f12> \\(.*\\) C-q C-j C-q C-j \\(\\(?:.* C-q C-j ?\\)*\\) <return> <return> <return> Poems <return>")))

;;; latex
(with-eval-after-load 'org
  (require 'ox-latex)
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-latex-hyperref-template "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c}, \n pdflang={%L},\n colorlinks=true,\n linkcolor=black}\n")
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  (setq org-format-latex-options
        '(:foreground default
                      :background default
                      :scale 1.5 :html-foreground "Black"
                      :html-background "Transparent"
                      :html-scale 1.0
                      :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
                                        ; pdf exporting
  (setq org-preview-latex-process-alist
        '((dvisvgm :programs
                   ("xelatex" "dvisvgm")
                   :description "xdv > svg"
                   :message "you need to install the programs: xelatex and dvisvgm."
                   :use-xcolor t
                   :image-input-type "xdv"
                   :image-output-type "svg"
                   :image-size-adjust (1.7 . 1.5)
                   :latex-compiler
                   ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                   :image-converter
                   ("dvisvgm %f -n -b min -c %S -o %O"))
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
                       ("convert -density %D -trim -antialias %f -quality 100 %O"))))

  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options '(("breaklines")
                                   ("bgcolor" "bg")))
  (setq org-latex-compiler "xelatex")
  ;; (add-to-list 'org-latex-packages-alist
  ;;            '("UTF8" "ctex" t))
  (add-to-list 'org-latex-packages-alist
	           '("cache=false" "minted" t))
  (add-to-list 'org-latex-packages-alist
	           '("" "xcolor" t))
  (add-to-list 'org-latex-packages-alist
	           '("" "tikz"))
  (setq org-latex-pdf-process
	    '("xelatex -8bit --shell-escape -interaction nonstopmode -output-directory=%o %f"
	      "biber %b"
	      "xelatex -8bit --shell-escape -interaction nonstopmode -output-directory=%o %f"
	      "xelatex -8bit --shell-escape -interaction nonstopmode -output-directory=%o %f"
	      "rm -fr %b.out %b.log %b.tex %b.brf %b.bbl auto"
	      ))
  (add-to-list 'org-latex-classes
	           '("beamer"
		         "\\documentclass[ignorenonframetext,presentation]{beamer}"
		         ("\\section{%s}" . "\\section*{%s}")
		         ("\\subsection{%s}" . "\\subsection*{%s}")))
  (add-to-list 'org-latex-classes
	           '("article_cn"
		         "\\documentclass[11pt]{ctexart}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]
\\definecolor{bg}{rgb}{0.95,0.95,0.95}"
		         ("\\section{%s}" . "\\section*{%s}")
		         ("\\subsection{%s}" . "\\subsection*{%s}")
		         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		         ("\\paragraph{%s}" . "\\paragraph*{%s}")
		         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


;;; pandoc support
(elemacs-require-package 'ox-pandoc)
(with-eval-after-load 'ox
  (require 'ox-pandoc)
  (setq org-pandoc-options-for-docx '((standalone . nil)))
  (defun org-pandoc-link (link contents info)
    "Transcode a LINK object.

The registered formatter for the 'pandoc backend is used. If none
exists, transcode using the registered formatter for the 'org
export backend. For fuzzy (internal) links, resolve the link
destination in order to determine the appropriate reference
number of the target Table/Figure/Equation etc. CONTENTS is the
description of the link, as a string, or nil. INFO is a plist
holding contextual information."
    (let ((type (org-element-property :type link)))
      (cond
       ;; Try exporting with a registered formatter for 'pandoc
       ((org-export-custom-protocol-maybe link contents 'pandoc))
       ;; Try exporting with a registered formatter for 'org
       ((org-export-custom-protocol-maybe link contents 'org))

       ;; Otherwise, override fuzzy (internal) links that point to
       ;; numbered items such as Tables, Figures, Sections, etc.
       ((string= type "fuzzy")
	    (let* ((path (org-element-property :path link))
               (destination (org-export-resolve-fuzzy-link link info))
               (dest-type (when destination (org-element-type destination)))
               (number nil))
          ;; Different link targets require different predicates to the
          ;; `org-export-get-ordinal' function in order to resolve to
          ;; the correct number. NOTE: Should be the same predicate
          ;; function as used to generate the number in the
          ;; caption/label/listing etc.
          (cond
           ((eq dest-type 'paragraph)   ; possible figure
            (setq number (org-export-get-ordinal
                          destination info nil #'org-html-standalone-image-p)))

           ((eq dest-type 'latex-environment)
            (setq number (org-export-get-ordinal
                          destination info nil
                          #'org-pandoc--numbered-equation-p)))

           ((eq dest-type 'has-caption) ;; captioned items
            (setq number (org-export-get-ordinal
                          destination info nil #'org-pandoc--has-caption-p))
	        ))

          ;; Numbered items have the number listed in the link
          ;; description, , fall back on the text in `contents'
          ;; if there's no resolvable destination
          (cond
           ;; Numbered items have the number listed in the link description
           (number
            (format "[[#%s][%s]]" path
                    (if (atom number) (number-to-string number)
                      (mapconcat #'number-to-string number ".")))
	        )

           ;; Unnumbered headlines have the heading name in the link
           ;; description
           ((eq dest-type 'headline)
            (format "[[#%s][%s]]" path
                    (org-export-data
                     (org-element-property :title destination) info)))

           ;; No resolvable destination, fallback on the text in `contents'
           ((eq destination nil)
            (when (org-string-nw-p contents) contents))

           ;; Valid destination, but without a numbered caption/equation
           ;; and not a heading, fallback to standard org-mode link format
           (t
            (org-element-link-interpreter link contents)))))

       ;; Otherwise, fallback to standard org-mode link format
       ((org-element-link-interpreter link contents))))))

;;; mixed pitch mode
(elemacs-require-package 'mixed-pitch)
(with-eval-after-load 'org
  (require 'mixed-pitch)
  (add-hook 'org-mode-hook #'mixed-pitch-mode)
  (setq mixed-pitch-variable-pitch-cursor 'box
        mixed-pitch-set-height t)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-date))

(add-to-list 'load-path "~/.emacs.d/site-lisp/svg-tag-mode/")
(with-eval-after-load 'org
  (require 'svg-tag-mode)
  (setq svg-lib-style-default
        '(:background "#F5F5F5" :foreground "#37474f" :padding 1 :margin 0
                      :stroke 2 :radius 5 :alignment 0.5 :width 20 :height 0.9
                      :scale 0.75 :crop-left nil :crop-right nil
                      :collection "material" :font-family "Cascadia Mono"
                      :font-size 11 :font-weight regular :ascent 14))
  
  (setq svg-tag-action-at-point 'edit)
  (setq svg-lib-icon-collections
        '(("bootstrap" .
           "https://icons.getbootstrap.com/assets/icons/%s.svg")
          ("simple" .
           "https://raw.githubusercontent.com/simple-icons/simple-icons/develop/icons/%s.svg")
          ("material" .
           "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
          ("octicons" .
           "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
          ("boxicons" .
           "https://boxicons.com/static/img/svg/regular/bx-%s.svg")))
  
  (defun svg-lib-tag (label &optional style &rest args)
    "Create an image displaying LABEL in a rounded box using given STYLE
and style elements ARGS."

    (let* ((default svg-lib-style-default)
           (style (if style (apply #'svg-lib-style nil style) default))
           (style (if args  (apply #'svg-lib-style style args) style))

           (foreground  (plist-get style :foreground))
           (background  (plist-get style :background))

           (crop-left   (plist-get style :crop-left))
           (crop-right  (plist-get style :crop-right))

           (alignment   (plist-get style :alignment))
           (stroke      (plist-get style :stroke))
           ;; (width       (plist-get style :width))
           (height      (plist-get style :height))
           (radius      (plist-get style :radius))
           ;; (scale       (plist-get style :scale))
           (margin      (plist-get style :margin))
           (padding     (plist-get style :padding))
           (font-size   (plist-get style :font-size))
           (font-family (plist-get style :font-family))
           (font-weight (plist-get style :font-weight))
           
           ;; use `fixed-pitch' while in `mixed-pitch-mode'
           (txt-char-width  (window-font-width nil 'fixed-pitch))
           (txt-char-height (window-font-height nil 'fixed-pitch))
           (txt-char-height (if line-spacing
                                (+ txt-char-height line-spacing)
                              txt-char-height))
           (font-info       (font-info (format "%s-%d" font-family font-size)))
           (font-size       (aref font-info 2)) ;; redefine font-size
           ;; (ascent          (aref font-info 8))
           (ascent          (plist-get style :ascent))
           (tag-char-width  (aref font-info 11))
           ;; (tag-char-height (aref font-info 3))
           (tag-width       (* (+ (length label) padding) txt-char-width))
           (tag-height      (* txt-char-height height))

           (svg-width       (+ tag-width (* margin txt-char-width)))
           (svg-height      tag-height)

           (tag-x  (* (- svg-width tag-width)  alignment))
           (text-x (+ tag-x (/ (- tag-width (* (length label) tag-char-width))
                               2)))
           (text-y ascent)

           (tag-x      (if crop-left  (- tag-x     txt-char-width) tag-x))
           (tag-width  (if crop-left  (+ tag-width txt-char-width) tag-width))
           (text-x     (if crop-left  (- text-x (/ stroke 2)) text-x))
           (tag-width  (if crop-right (+ tag-width txt-char-width) tag-width))
           (text-x     (if crop-right (+ text-x (/ stroke 2)) text-x))
           
           (svg (svg-create svg-width svg-height)))

      (if (>= stroke 0.25)
          (svg-rectangle svg tag-x 0 tag-width tag-height
                         :fill foreground :rx radius))
      (svg-rectangle svg (+ tag-x (/ stroke 2.0)) (/ stroke 2.0)
                     (- tag-width stroke) (- tag-height stroke)
                     :fill background :rx (- radius (/ stroke 2.0)))
      (svg-text svg label
                :font-family font-family :font-weight font-weight
                :font-size font-size :fill foreground :x text-x :y  text-y)
      (svg-lib--image svg :ascent 'center)))
  
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}\\(?: [.+]?\\+[0-9]+[dwmy]\\)?")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0) nil
                                      :margin 0 :stroke 2 :radius 3
                                      :padding 2 :width 4 :height 0.4
                                      :foreground "#B0BEC5")
                (svg-lib-tag (concat value "%") nil
                             :stroke 0 :margin 0 :foreground "#384d57"
                             :ascent 12))
               :ascent 70 :scale 0.7))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 3
                                        :padding 2 :width 4 :height 0.4
                                        :foreground "#B0BEC5")
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0 :foreground "#384d57"
                               :ascent 12))
                 :ascent 70 :scale 0.7)))

  (setq svg-tag-tags
        `(
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
          ("TODO" . ((lambda (tag)
                       (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0
                                     :height 1.1 :ascent 16))))
          ("NEXT" . ((lambda (tag)
                       (svg-tag-make "NEXT" :face 'mindre-keyword :margin 0
                                     :height 1.1 :ascent 16))))
          ("STARTED" . ((lambda (tag)
                          (svg-tag-make "STARTED" :face 'mindre-keyword
                                        :margin 0 :height 1.1 :ascent 16))))
          ("PROJECT" . ((lambda (tag)
                          (svg-tag-make "PROJECT" :face 'mindre-keyword
                                        :margin 0 :height 1.1 :ascent 16))))
          ("DONE" . ((lambda (tag)
                       (svg-tag-make "DONE" :face 'mindre-faded :inverse t
                                     :margin 0 :height 1.1 :ascent 16))))
          ("FIXME" . ((lambda (tag)
                        (svg-tag-make "FIXME" :face 'mindre-critical :margin 0
                                      :height 1.1 :ascent 16))))


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
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil
                            :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t
                            :crop-left t :margin 0 :face 'org-date))))))
  
  (add-hook 'org-mode-hook (lambda ()
                             (make-local-variable 'font-lock-extra-managed-props)
                             (svg-tag-mode)))

  (defun eli-org-agenda-show-svg ()
    (let* ((n 0)
           (case-fold-search nil)
           (keywords (mapcar #'svg-tag--build-keywords svg-tag--active-tags))
           (keyword (nth n keywords)))
      (while keyword
        (save-excursion
          (while (re-search-forward (nth 0 keyword) nil t)
            (overlay-put (make-overlay
                          (match-beginning 0) (match-end 0))
                         'display  (nth 3 (eval (nth 2 keyword)))) ))
        (setq n (1+ n))
        (setq keyword (nth n keywords)))))
  (add-hook 'org-agenda-finalize-hook #'eli-org-agenda-show-svg))

(provide 'init-org)
;;; init-org.el ends here.
