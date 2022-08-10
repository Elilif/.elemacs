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
(with-eval-after-load 'org
  ;; hide drawers
  ;; ((eq org-cycle-subtree-status 'subtree)
  ;;  (org-show-subtree)
  ;;  (org-unlogged-message "ALL")
  ;;  (setq org-cycle-subtree-status 'all))
  ;; add above codes before ((or children-skipped in org-cycle-internal-local
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
  ;; renumbering and sorting footnotes automatically after each deletion or insertion
  (defun eli/clock-in-to-nest (kw)
    (if (org-get-todo-state)
	"STARTED"))
  (setq org-footnote-auto-adjust t)
  (setq org-clock-in-switch-to-state `eli/clock-in-to-nest)
  (setq org-clock-mode-line-total 'today)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-continuously t)
  ;; org todo keaywords
  (setq org-todo-keywords
	(quote ((sequence "TODO(t/!)" "STARTED(s)" "|" "DONE(d!/!)")
		(sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
		(sequence "WAITING(w@/!)" "NEXT(n!/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)"))))

  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "✎")
					                     ("#+END_SRC" . "□")
					                     ("#+begin_src" . "✎")
					                     ("#+end_src" . "□")
					                     ("[ ]" . "🞎")
                                         ("[-]" . "🞔")
					                     ("[X]" . "🗹")
					                     ("#+begin_quote" . "»")
					                     ("#+end_quote" . "«")
					                     ("#+begin_verse" . "ζ")
					                     ("#+end_verse" . "ζ")
					                     ("#+begin_example" . "")
					                     ("#+end_example" . "")
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
	         (scheduled-repeat (org-get-repeat (org-entry-get (point) "SCHEDULED")))
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
	          (error "Habit %s deadline repeat period is less than or equal to scheduled (%s)"
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
	    (if (= (time-convert (time-subtract (nth  counter closed-days) (nth (1- counter) closed-days)) 'integer) 1)
	        (progn (setq streaks (1+ streaks)))
	      (if (> streaks max-streaks)
	          (progn (setq max-streaks streaks)
		             (setq streaks 1)))
	      )
	    (setq counter (1+ counter)))
      (setq counter (1- counter))
      (if (= (time-convert (time-subtract today (nth counter closed-days)) 'integer) 1)
	      (progn (setq current-streaks (1+ current-streaks))
		         (while (= (time-convert (time-subtract (nth  counter closed-days) (nth (1- counter) closed-days)) 'integer) 1)
		           (setq current-streaks (1+ current-streaks))
		           (setq counter (1- counter)))
		         )

	    )
      (if (> streaks max-streaks)
	      (setq max-streaks streaks))
      (insert " (" (number-to-string current-streaks) "/" (number-to-string max-streaks) "/" (number-to-string sum) ")")
      ))
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
  (keymap-set org-mode-map "C-c C-l" #'org-insert-link)
  (keymap-set org-mode-map "<remap> <org-cycle-agenda-files>" #'avy-goto-char)
  (keymap-set org-mode-map "C-<tab>" #'eli/org-expand-all))



;;; agenda
(with-eval-after-load 'org
  (setq org-agenda-span 'day)
  (setq org-agenda-show-inherited-tags nil)
  (setq org-agenda-window-setup 'only-window)
  (with-eval-after-load 'org
    (elemacs-require-package 'org-reverse-datetree))

  ;; custom org agenda view
  (setq org-agenda-log-mode-items '(clock))
  (setq org-agenda-log-mode-add-notes nil)

  (setq org-agenda-custom-commands
	    '(("g" "GTD"
           ((agenda "" nil)
            (tags-todo  "/+TODO"
			            ((org-agenda-overriding-header "Inbox")
			             (org-tags-match-list-sublevels t)
			             (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
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
              ;; Delete file from dynamic files when all TODO entry changed to DONE
              (unless (search-forward-regexp org-not-done-heading-regexp nil t)
		        (customize-save-variable
		         'dynamic-agenda-files
		         (cl-delete-if (lambda (k) (string= k file))
			                   dynamic-agenda-files))))
          ;; Add this file to dynamic agenda files
          (unless (member file dynamic-agenda-files)
            (customize-save-variable 'dynamic-agenda-files
                                     (add-to-list 'dynamic-agenda-files file)))))))

  (defun dynamic-agenda-files-advice (orig-val)
    (cl-union orig-val dynamic-agenda-files :test #'equal))

  (advice-add 'org-agenda-files :filter-return #'dynamic-agenda-files-advice)
  (with-eval-after-load 'org
    (add-to-list 'org-after-todo-state-change-hook 'update-dynamic-agenda-hook t)))


;;; capture
;; org capture
(with-eval-after-load 'org
  (setq org-agenda-dir "~/Dropbox/org")
  (setq org-directory "~/Dropbox/org")

  (setq org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir))
  (setq org-agenda-file-projects (expand-file-name "projects.org" org-agenda-dir))
  (setq org-agenda-file-habit (expand-file-name "daily.org" org-agenda-dir))
  (setq org-agenda-file-notes (expand-file-name "notes.org" org-agenda-dir))
  (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
  (setq org-agenda-file-te (expand-file-name "words.org" org-agenda-dir))
  (setq org-agenda-file-lists (expand-file-name "lists.org" org-agenda-dir))
  (setq org-agenda-files '("~/Dropbox/org/journal.org" "/home/eli/Dropbox/org/古文.org" "/home/eli/Dropbox/org/Français.org" "/home/eli/Dropbox/org/daily.org" "/home/eli/Dropbox/org/lists.org" "/home/eli/Dropbox/org/inbox.org" "/home/eli/Dropbox/org/words.org" "/home/eli/Dropbox/org/projects.org"))
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

  ;; from: https://stackoverflow.com/questions/21073859/is-there-a-way-with-org-capture-templates-to-not-insert-a-line-if-initial-conten
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

  ;; better fill region in capture
  (defun eli/fill-region ()
    (save-excursion
      (push-mark)
      (push-mark (point-max) nil t)
      (goto-char (minibuffer-prompt-end))
      (org-fill-paragraph nil t)))
  (add-hook 'org-capture-prepare-finalize-hook 'eli/fill-region)


  ;; dedicated to "event" template
  (defun eli-org-capture-template-goto-today ()
    "Set point for capturing at what capture target file+headline with headline set to %l would do."
    (org-capture-put :target (list 'file+headline (nth 1 (org-capture-get :target)) (format-time-string "%Y-%m-%d")))
    (org-capture-put-target-region-and-position)
    (widen)
    (let ((hd (nth 2 (org-capture-get :target))))
      (goto-char (point-min))
      (if (re-search-forward
	       (format org-complex-heading-regexp-format (regexp-quote hd))
	       nil t)
	      (goto-char (point-at-bol))
	    (goto-char 361)
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
       "* TODO %?\nSCHEDULED: <%(org-read-date nil nil \"+0d\") .+1d>\n:PROPERTIES:\n:STYLE:    habit\n:END:\n\n%U"
       :empty-lines 0)
      ("n" "Notes" entry (file+headline org-agenda-file-inbox "Notes")
       "* %?\n%(v-i-or-nothing)\n%(v-a-or-nothing)\n%U"
       :empty-lines 0
	   :prepend t)
      ("j" "Journals" entry (file+function org-agenda-file-journal org-reverse-datetree-goto-date-in-file)
       "* %<%H:%M> %?"
       :empty-lines 1
	   :prepend t
	   :clock-resume t
	   :clock-in t
	   )
	  ("e" "Events" entry (file+function "~/Elilif.github.io/Eli's timeline.org"  eli-org-capture-template-goto-today)
	   "* %?"
	   )
	  ("B" "Blogs" plain (file eli/capture-report-date-file)
	   "#+TITLE: %?\n#+DATE: %<%Y-%m-%d>\n#+STARTUP: showall\n#+OPTIONS: toc:nil H:2 num:2\n"
	   )
	  ("T" "Time Report" plain (file+function "~/Dropbox/org/Clock_Report.org"  org-reverse-datetree-goto-date-in-file)
	   "#+BEGIN: clocktable :scope agenda-with-archives :maxlevel 6 :block %<%Y-%m-%d> :fileskip0 t :indent t :link t :formula % :sort (3 . ?T)\n#+END:"
	   :empty-lines 0
	   :jump-to-captured t)
      ;; ("d" "Digests" entry (file+olp+datetree org-agenda-file-notes)
      ;;  "* %a \n%i \n%U"
      ;;  :empty-lines 0)
	  ("w" "Words" entry (file org-agenda-file-te)
	   "* TODO %u [/]\nSCHEDULED: <%(org-read-date nil nil \"+1d\") .+1d>\n%?"
	   :jump-to-captured t)
	  ("f" "Français" entry (file "~/Dropbox/org/Français.org")
	   "* TODO %u [/]\nSCHEDULED: <%(org-read-date nil nil \"+1d\") .+1d>\n%?"
	   :jump-to-captured t)
	  ("g" "古文" entry (file "~/Dropbox/org/古文.org")
	   "* TODO %u [/]\nSCHEDULED: <%(org-read-date nil nil \"+1d\") .+1d>\n%?"
	   :jump-to-captured t)
	  ("b" "Book" entry (file+headline org-agenda-file-lists "Books")
	   "* %?\n  %^{Title}p %^{Isbn}p %^{Types}p %^{Authors}p %^{Translator}p %^{Publisher}p %^{Nation}p %^{Lang}p %^{Rating}p")
	  ("m" "Movies and Musicals" entry (file+headline org-agenda-file-lists "Movies and Musicals")
	   "* %?\n %^{Title}p %^{IMDB}p %^{URL}p %^{Director}p %^{Writer}p %^{Actors}p %^{Types}p %^{Time}p %^{Release}p %^{Nation}p %^{Lang}p %^{Rating}p")
	  ("s" "Series" entry (file+headline org-agenda-file-lists "Series")
	   "* %?\n %^{Title}p %^{IMDB}p %^{URL}p %^{Director}p %^{Writer}p %^{Actors}p %^{Types}p %^{Time}p %^{Episodes}p %^{Release}p %^{Nation}p %^{Lang}p %^{Rating}p")
	  ("a" "Animes" entry (file+headline org-agenda-file-lists "Animes")
	   "* %?\n %^{Title}p %^{URL}p %^{Episodes}p %^{Release}p %^{Director}p %^{Authors}p %^{Publisher}p %^{Rating}p")
	  ("r" "NOTE" entry (file "~/Dropbox/org/roam/inbox.org")
	   "* %?\n%(v-i-or-nothing)\n%(v-a-or-nothing)"
	   :create-id t)
      ))
  )

;;; time report
;; from: https://emacs.stackexchange.com/questions/31683/schedule-org-task-for-last-day-of-every-month
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
	(notifications-notify :timeout (* appt-display-interval 60000) ;一直持续到下一次提醒
			      :title (format "%s分钟内有新的任务" min-to-appt)
			      :body appt-msg)
      (dolist (i (number-sequence 0 (1- (length min-to-appt))))
	(notifications-notify :timeout (* appt-display-interval 60000) ;一直持续到下一次提醒
			      :title (format "%s分钟内有新的任务" (nth i min-to-appt))
			      :body (nth i appt-msg)))))
  )

;;; org-refile
(with-eval-after-load 'org
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets
	'((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))
  )

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
	 (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
           (string-match-p "\\cl$" string))))

  (setq eli/prefer-English t)
  (defun eli/input-switch ()
    (interactive)
    (if (not eli/prefer-English)
	(progn
	  (setq rime-disable-predicates '(rime-predicate-prog-in-code-p
					  rime-predicate-space-after-ascii-p
					  rime-predicate-after-ascii-char-p
					  +rime-predicate-punctuation-line-begin-p
					  rime-predicate-org-in-src-block-p
					  rime-predicate-space-after-cc-p
					  rime-predicate-current-uppercase-letter-p
					  rime-predicate-hydra-p ))
	  (setq eli/prefer-English t))
      (progn
	(setq rime-disable-predicates
	      (seq-difference rime-disable-predicates '(rime-predicate-space-after-ascii-p
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
	rime-inline-ascii-trigger 'shift-l)
  (keymap-global-set "C-s-k" #'rime-inline-ascii)
  (keymap-global-set "C-s-j" #'+rime-convert-string-at-point)
  (add-hook 'org-mode-hook 'toggle-input-method)
  )

(elemacs-require-package 'org-superstar)
(setq org-superstar-headline-bullets-list '("☰" "○" "✸" "✤" "◆" "✜" "▶"))
(add-hook 'org-mode-hook #'org-superstar-mode)


;;; roam
(elemacs-require-package 'org-roam)
(with-eval-after-load 'org
  (setq org-roam-directory "~/Dropbox/org/roam/")
  (setq org-roam-db-gc-threshold most-positive-fixnum
	org-id-link-to-org-use-id 'create-if-interactive)
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.4)
		 (window-height . fit-window-to-buffer)))
  (setq org-roam-mode-section-functions
	(list #'org-roam-backlinks-section
	      #'org-roam-reflinks-section
	      ;; #'org-roam-unlinked-references-section
	      ))
  (setq org-roam-completion-everywhere t)

  (setq org-roam-dailies-capture-templates
	'(("d" "default" entry
           "* %?"
           :if-new (file+datetree "~/Dropbox/org/roam/daily/dailies.org" day))))
  (setq org-roam-capture-templates '(("m" "main" plain "%?"
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
	       #'org-roam-db-autosync-mode)
  (with-eval-after-load 'org-roam
    ;; Codes blow are used to general a hierachy for title nodes that under a file
    (cl-defmethod org-roam-node-doom-filetitle ((node org-roam-node))
      "Return the value of \"#+title:\" (if any) from file that NODE resides in.
      If there's no file-level title in the file, return empty string."
      (or (if (= (org-roam-node-level node) 0)
              (org-roam-node-title node)
            (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
	  ""))
    (cl-defmethod org-roam-node-doom-hierarchy ((node org-roam-node))
      "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
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
                     separator (propertize (string-join olp " > ") 'face '(shadow italic))
                     separator title)))))

    (cl-defmethod org-roam-node-type ((node org-roam-node))
      "Return the TYPE of NODE."
      (condition-case nil
	  (file-name-nondirectory
	   (directory-file-name
            (file-name-directory
             (file-relative-name (org-roam-node-file node) org-roam-directory))))
	(error "")))

    (setq org-roam-node-display-template (concat "${type:15} ${doom-hierarchy:120} " (propertize "${tags:*}" 'face 'org-tag))))

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
	    org-capture-templates-contexts '(("c" (org-mru-clock-capturing))
                                         ("1" (elemacs-global-interactive-capture))
                                         ("2" (elemacs-global-interactive-capture))
                                         ))
  )

(elemacs-require-package 'org-clock-convenience)
(setq org-clock-convenience-clocked-agenda-re "^ +\\([^:]+\\):[[:space:]]*\\(\\([ 	012][0-9]\\):\\([0-5][0-9]\\)\\)-\\(\\([ 012]*[0-9]\\):\\([0-5][0-9]\\)\\|.*\\)?[[:space:]]+Clocked:[[:space:]]+\\(([0-9]+:[0-5][0-9])\\|(-)\\)")
(with-eval-after-load 'org-agenda
  (keymap-set org-agenda-mode-map "M-<up>" #'org-clock-convenience-timestamp-up)
  (keymap-set org-agenda-mode-map "M-<down>" #'org-clock-convenience-timestamp-down)
  (keymap-set org-agenda-mode-map "<f6>" #'org-clock-convenience-fill-gap)
  (keymap-set org-agenda-mode-map "<f7>" #'org-clock-convenience-fill-gap-both))

;; fix M-j
(defun eli-org-fill-prefix ()
  "Set `fill-prefix' to the empty string."
  (setq fill-prefix ""))
(add-hook 'org-mode-hook #'eli-org-fill-prefix)


;;; org-download
;; Allow Emacs to access content from clipboard.
(setq x-select-enable-clipboard t
      x-select-enable-primary nil)

(with-eval-after-load 'org
  (defun make-orgcapture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "org-capture") (window-system . x)))
    (select-frame-by-name "org-capture")
    (org-capture)
    (delete-other-windows))

  (elemacs-require-package 'org-download)
  (setq-default org-download-method 'directory
		org-download-image-dir "~/Documents/org-images"
		org-download-heading-lvl nil
		org-download-delete-image-after-download t
		org-download-screenshot-method "flameshot gui --raw > %s"
		org-download-image-org-width 800
		org-download-annotate-function (lambda (link) "") ;; Don't annotate
		)

  ;; org-download use buffer-local variables. Set it individually in files. Otherwise, put things flatly in misc
  ;; folder.

  (add-hook 'dired-mode-hook 'org-download-enable)
  (global-set-key (kbd "C-c l") 'org-store-link) ;; crop in X11 first, and paste within here later
  ;; Use #+ATTR_ORG: :width 300px to customized image display width
  (setq org-image-actual-width nil)
  ;; org-attach method
  (setq-default org-attach-method 'mv
		org-attach-auto-tag "attach"
		org-attach-store-link-p 't)
  )

(provide 'init-org)
;;; init-org.el ends here.
