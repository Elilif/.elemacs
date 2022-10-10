;; init-hydra.el --- Initialize hydra configurations.	-*- lexical-binding: t -*-

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

(keymap-global-set "C-c o" #'hydra-org/body)
(keymap-global-set "C-c w" #'jp-window/body)
(keymap-global-set "C-c b" #'hydra-bibtex/body)
(keymap-global-set "C-c s" #'hydra-search/body)
(keymap-global-set "C-c m" #'hydra-player/body)
(keymap-global-set "C-c e" #'hydra-edit/body)
(keymap-global-set "C-c r" #'hydra-reader/body)
(keymap-global-set "C-c d" #'hydra-develop/body)
(keymap-global-set "C-c q" #'hydra-emacs/body)
(keymap-global-set "C-c j" #'hydra-roam/body)
(keymap-global-set "C-c i" #'hydra-insert/body)
(keymap-global-set "C-c p" #'hydra-move/body)
(keymap-global-set "C-c [" #'hydra-skan-user-buffers-prev/body)
(keymap-global-set "C-c ]" #'hydra-skan-user-buffers-next/body)
(keymap-global-set "C-c n" #'hydra-org-noter/body)
(keymap-global-set "C-c a" #'hydra-org-agenda/body)

(pretty-hydra-define hydra-search
  (:color amaranth :exit t :quit-key "q"
	  :pre (progn (setq which-key-inhibit t))
	  :post (progn (setq which-key-inhibit nil) ))
  ("Dicts"
   (("t" Eli/te-search "search TE")
    ("d" Eli/dict-search "search Dicts"))
   "Deft"
   (("n" notdeft "notdeft"))
   "git grep"
   (("g" consult-git-grep))
   "Google"
   (("s" my-search-with-chrome))
   ))
(pretty-hydra-define hydra-edit
  (:color amaranth :exit t :quit-key "q"
	  :pre (progn (setq which-key-inhibit t))
	  :post (progn (setq which-key-inhibit nil)))
  ("multiple cursors"
   (("l" mc/edit-lines "edit-lines")
    ("a" mc/mark-all-like-this-dwim "mark all dwim")
    ("d" mc/mark-all-symbols-like-this "mark all")
    ("s" set-rectangular-region-anchor "set mc")
    ("n" mc/insert-numbers "insert numbers"))
   "Format"
   (("f" smart-align "align"))))

(pretty-hydra-define hydra-move
  (:color amaranth :exit t :quit-key "q"
	      :pre (progn (setq which-key-inhibit t))
	      :post (progn (setq which-key-inhibit nil)))
  ("Moving"
   (("a" sp-beginning-of-sexp)
    ("e" sp-end-of-sexp)
    ("f" sp-forward-sexp)
    ("b" sp-backward-sexp)
    ("n" sp-down-sexp)
    ("N" sp-backward-down-sexp)
    ("p" sp-up-sexp)
    ("P" sp-backward-up-sexp))
   "Slurping & barfing"
   (("h" sp-backward-slurp-sexp)
    ("H" sp-backward-barf-sexp)
    ("l" sp-forward-slurp-sexp)
    ("L" sp-forward-barf-sexp))
   "Wrapping"
   (("R" sp-rewrap-sexp)
    ("u" sp-unwrap-sexp)
    ("U" sp-backward-unwrap-sexp)
    ("(" sp-wrap-round)
    ("{" sp-wrap-curly)
    ("[" sp-wrap-square))
   "Sexp juggling"
   (("S" sp-split-sexp)
    ("s" sp-splice-sexp)
    ("r" sp-raise-sexp)
    ("j" sp-join-sexp)
    ("t" sp-transpose-sexp)
    ("A" sp-absorb-sexp)
    ("E" sp-emit-sexp)
    ("o" sp-convolute-sexp))
   "Destructive editing"
   (("c" sp-change-inner :exit t)
    ("C" sp-change-enclosing :exit t)
    ("k" sp-kill-sexp)
    ("K" sp-backward-kill-sexp)
    ("w" sp-copy-sexp))

   ))

(pretty-hydra-define hydra-roam
  (:color amaranth :exit t :quit-key "q"
	  :pre (progn (setq which-key-inhibit t))
	  :post (progn (setq which-key-inhibit nil) ))
  ("Roam"
   (("l" org-roam-buffer-toggle "toggle roam buffer")
    ("f" org-roam-node-find "find roam node")
    ("n" org-id-get-create "create roam id")
    ("i" org-roam-node-insert "insert roam node")
    ("s" helm-org-roam-files-headings "search headings")
    ("w" org-roam-refile "refile roam node"))
   "Roam"
   (("c" org-roam-dailies-capture-today "roam capture")
    ("ra" org-roam-ref-add "add refs")
    ("rd" org-roam-ref-remove "remove a ref")
    ("t" org-roam-tag-add "add tags")
    ("v" org-roam-tag-remove "remove a tag")
    ("h" yuchen/helm-org-run-marked-heading-id-link "insert a headline"))
   "Roam"
   (("dd" org-roam-dailies-goto-today "today")
    ("df" org-roam-dailies-goto-date "goto date")
    ("dc" org-roam-dailies-capture-today "goto today")
    ("aa" org-roam-alias-add "add alias")
    ("ar" org-roam-alias-remove "remove alias")
    ("u" org-roam-ui-mode))
   )
  )

(pretty-hydra-define hydra-emacs
  (:color amaranth :exit t :quit-key "q" :idle 2
	  :pre (progn (setq which-key-inhibit t))
	  :post (progn (setq which-key-inhibit nil) ))
  ("basic"
   (("E" eval-buffer)
    ("f" (let ((default-directory "~/.emacs.d/lisp/"))
           (call-interactively 'find-file)) "config files")
    ("R" restart-emacs)
    )
   "Bookmark"
   (("bs" bookmark-set "set bookmark")
    ("bj" consult-bookmark "jump bookmark")
    ("bd" bookmark-delete "delete bookmark"))
   "edit"
   (("ed" mc/mark-all-dwim "mark all dwim")
    ("ee" mc/edit-lines "elit lines"))
   "treemacs"
   (("t" treemacs "treemacs"))
   ))

(pretty-hydra-define hydra-org
  (:color amaranth :exit t :quit-key "q"
	  :pre
	  (progn (setq which-key-inhibit t))
	  :post (progn (setq which-key-inhibit nil) ))
  ("Basic"
    (("h" org-mode "org mode")
    ("j" consult-org-heading "headings")
    ("f" my/consult-org-file "agenda filter"))
   "Org link"
   (("li" grab-x-link-chromium-insert-link "insert web link")
    ("lo" grab-x-link-chromium-insert-org-link "insert org link")
    ("ld" org-download-clipboard "org download clipboard"))
   "Org-clock"
   (("i" org-clock-in)
    ("c" org-mru-clock-in)
    ("o" org-clock-out)

    ("ke" org-clock-modify-effort-estimate "modify effort estimates")
    ("kk" org-clock-cancel)

    ("g" org-mru-clock-goto)
    ("d" org-clock-display)
    ("kr" org-clock-report)
    ("?" (org-info "Clocking commands")))
   "Org-timer"
   (("tr" org-timer-start)
    ("tn" org-timer-set-timer)
    ("tp" org-timer-pause-or-continue "Pause/Continue")
    ("ts" org-timer-stop)
    ("tm" org-timer)
    ("tt" org-timer-item)
    ("tz" (org-info "Timers") "info timers")
    )
   "Anki"
   (("ab" org-anki-browse-entry "browse")
    ("ac" org-anki-cloze-dwim "close")
    ("ad" org-anki-delete-entry "delete entry")
    ("aD" org-anki-delete-all "delete all")
    ("aS" org-anki-sync-all "sync all")
    ("as" org-anki-sync-entry "sync entry")
    ("au" org-anki-update-all "update all"))
   ""
   (("ax" org-anki-sync-checkbox "check boxes")
    ("al" org-anki-sync-description "description lists")
    ("ap" org-anki-sync-poem "poem")
    ("aw" org-anki-sync-word "word")
    ("ar" org-anki-sync-region "region"))
   "blog"
   (("bp" org-publish)
    ("bg" eli/push-to-gitpage)
    ("bt" org-timeline-export-to-html "export timeline"))
   ))

(pretty-hydra-define jp-window
  (:color amaranth :exit t :quit-key "q"
	  :pre (progn (setq which-key-inhibit t)  )
	  :post (progn (setq which-key-inhibit nil) ))
  ("Actions"
   (("TAB" other-window "switch")
    ("m" ace-delete-window "maximize")
    ("x" ace-delete-other-windows "delete")
    ("s" ace-swap-window "swap")
    ("a" ace-select-window "select"))

   ;; "Resize"
   ;; (("h" move-border-left "←")
   ;;  ("j" move-border-down "↓")
   ;;  ("k" move-border-up "↑")
   ;;  ("l" move-border-right "→")
   ;;  ("n" balance-windows "balance")
   ;;  ("f" toggle-frame-fullscreen "toggle fullscreen"))

   "Split"
   (("h" split-window-right "horizontally")
    ("v" split-window-below "vertically")
    )

   "Zoom"
   (("=" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("[" shrink-window-horizontally "h-shrink window" :exit nil)
    ("]" enlarge-window-horizontally "h-shrink window" :exit nil)
    ("b" balacne-windows "balacne windows")
    )))

;; Hydra for org agenda (graciously taken from Spacemacs)
;; (defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
;;                                  :post (setq which-key-inhibit nil)
;;                                  :hint none))
(pretty-hydra-define hydra-org-agenda
  (:color amaranth :exit t :quit-key "q"
	  :pre (progn (setq which-key-inhibit t)  )
	  :post (progn (setq which-key-inhibit nil) ))
  ("Entry"
   (("a" org-agenda)
    ("g" (org-agenda nil "g") "GTD")
    ("hA" org-agenda-archive-default "archive default")
    ("hk" org-agenda-kill "kill")
    ("hp" org-agenda-priority "priority")
    ("hr" org-agenda-refile "refile")
    ("h:" org-agenda-set-tags "set tage")
    ("ht" org-agenda-todo "todo"))
   "Visit entry"
   (("o"   link-hint-open-link "open link" :exit t)
    ("<tab>" org-agenda-goto "goto" :exit t)
    ("TAB" org-agenda-goto "goto" :exit t)
    ("SPC" org-agenda-show-and-scroll-up "show and scroll up")
    ("RET" org-agenda-switch-to "switch to" :exit t))
   "Date"
   (("dt" org-agenda-date-prompt "date-promt")
    ("dd" org-agenda-deadline "deadline")
    ("+" org-agenda-do-date-later "do date later")
    ("-" org-agenda-do-date-earlier "do date earlier")
    ("ds" org-agenda-schedule "schedule"))
   "View"
   (("vd" org-agenda-day-view "day view")
    ("vw" org-agenda-week-view "week view")
    ("vt" org-agenda-fortnight-view "fortnight view")
    ("vm" org-agenda-month-view "month view")
    ("vy" org-agenda-year-view "year view")
    ("vn" org-agenda-later "later")
    ("vp" org-agenda-earlier "earlier")
    ("vr" org-agenda-reset-view "reset view"))
   "Toggle mode"
   (("ta" org-agenda-archives-mode "archives mode")
    ("tA" (org-agenda-archives-mode 'files))
    ("tr" org-agenda-clockreport-mode "clockreport mode")
    ("tf" org-agenda-follow-mode "follow mode")
    ("tl" org-agenda-log-mode "log mode")
    ("td" org-agenda-toggle-diary "toggle diary"))
   "Filter"
   (("fc" org-agenda-filter-by-category "by category")
    ("fx" org-agenda-filter-by-regexp "by regexp")
    ("ft" org-agenda-filter-by-tag "by tag")
    ("fr" org-agenda-filter-by-tag-refine "by rag refine")
    ("fh" org-agenda-filter-by-top-headline "by top headline")
    ("fd" org-agenda-filter-remove-all "remove all"))
   "Clock"
   (("cq" org-agenda-clock-cancel "cancel")
    ("cj" org-agenda-clock-goto "goto" :exit t)
    ("ci" org-agenda-clock-in "clock in" :exit t)
    ("co" org-agenda-clock-out "clock out"))
   "Other"
   (("q" nil :exit t)
    ("sd" org-agenda-goto-date "goto date")
    ("." org-agenda-goto-today "goto today")
    ("rr" org-agenda-redo "redo"))))

(pretty-hydra-define hydra-bibtex
  (:color amaranth :exit t :quit-key "q"
	  :pre (progn (setq which-key-inhibit t))
	  :post (progn (setq which-key-inhibit nil) ))
  ("Reference"
   (("o" citar-open "citar open")
    ("s" ex/search-pdf-contents "search pdf")
    )
   "Calibre"
   (("t" eli/open-TE "open TE"))
   "Roam Bibtex"
   (("a" orb-note-actions "orb note actions")
    ("l" orb-insert-link "insert orb link")
    )
   "Bibtex"
   (("r" org-bibtex-read "org bibtex read")
    ("w" org-bibtex-write "org bibtex write"))
   ))

(pretty-hydra-define hydra-org-noter
  (:color amaranth :exit t :quit-key "q"
	  :pre (progn (setq which-key-inhibit t)  )
	  :post (progn (setq which-key-inhibit nil) ))
  ("Noter"
   (("n" org-noter "noter")
    ("c" org-noter-create-skeleton "create skeleton"))
   "Media note"
   (("p" org-media-note-play-online-video "online video")
    ("e" org-media-note-hydra/body "media note"))
   "PDF Annotation"
   (("l" pdf-annot-list-annotations "list annotations"))
   ))

(pretty-hydra-define hydra-reader
  (:color amaranth :exit t :quit-key "q"
	  :pre (progn (setq which-key-inhibit t)  )
	  :post (progn (setq which-key-inhibit nil) ))
  ("Elfeed"
   (("e" elfeed "elfeed"))
   "Mu4e"
   (("m" mu4e "Mails"))
   ))

(pretty-hydra-define hydra-skan-user-buffers-next
  (:body-pre (next-buffer)
	     :hint nil
	     :quit-key "q"
	     :pre (progn (setq which-key-inhibit t)  )
	     :post (progn (setq which-key-inhibit nil) ))
  ("skan user buffers"
   (("\]" next-buffer)
    ("\[" previous-buffer)
    ("k" kill-this-buffer)
    ("q" nil))))

(pretty-hydra-define hydra-skan-user-buffers-prev
  (:body-pre (previous-buffer)
	     :hint nil
	     :quit-key "q"
	     :pre (progn (setq which-key-inhibit t)  )
	     :post (progn (setq which-key-inhibit nil) ))
  ("skan user buffers"
   (("\]" next-buffer)
    ("\[" previous-buffer)
    ("k" kill-this-buffer)
    ("q" nil))))

(pretty-hydra-define hydra-insert
  (:color amaranth :exit t :quit-key "q"
	  :pre (progn (setq which-key-inhibit t))
	  :post (progn (setq which-key-inhibit nil)
		       ))
  ("Input"
   (("s" eli/input-switch "switch input")
    ("k" eli/insert-key-sequence "insert key sequence"))
   "Yank"
   (("p" consult-yank-pop "Clipboard"))
   "Emoji & Pic"
   (("i" emoji-insert)
    ("f" emoji-search)
    ("m" eli/select-images "images"))
   "LaTeX"
   (("l" mathpix-screenshot "mathpix")
    ("r" eli/org-ref-insert-ref-link "insert org ref"))))

(pretty-hydra-define hydra-player
  (:color amaranth :exit t :quit-key "q"
	  :pre (progn (setq which-key-inhibit t))
	  :post (progn (setq which-key-inhibit nil)
		       ))
  ("Playlists"
   (("e" emms)
    ;; ("g" emms-play-directory "open dir")
    ("a" consult-emms-library "add library")
    ("m" emms-metaplaylist-mode-go "metaplaylist")
    ("c" consult-emms-current-playlist "choose to play")
    )
   "Controls"
   (("n" emms-next "next" :exit nil)
    ("p" emms-previous "previous" :exit nil)
    ("x" emms-shuffle "shuffle")
    ("i" emms-show "song info")
    ("-" emms-volume-lower "lower volume" :exit nil)
    )
   "Controls"
   (("SPC" emms-pause "pause")
    ("r" emms-toggle-repeat-track "repeat")
    ("," emms-seek-backward "backward" :exit nil)
    ("." emms-seek-forward "forward" :exit nil)
    ;; ("d" emms-play-dired "play the list")
    ("=" emms-volume-raise "raise volume" :exit nil)
    )
   "lyrics"
   (("l" lyrics-fetcher-show-lyrics "lyrics")
    ("C" lyrics-fetcher-lyrics-catchup "lyrics catchup")
    ("t" emms-lyrics-toggle-display-on-minibuffer "toggle lyrics")
    )
   "Browser"
   (("b" emms-browser "browser")
    ("o" emms-browser-show-file-on-line "show score")
    ("d" emms-browser-set-score "set score")
    )
   "Score"
   (("ss" emms-score-set-playing "set score for playing track")
    ("sd" emms-score-show-playing "show score of playing track")
    ("st" emms-score-set-tolerance "tolerance")
    ("f"  eli/emms-filter "filter"))
   ))
(pretty-hydra-define hydra-develop
  (:color amaranth :exit t :quit-key "q"
	  :pre (progn (setq which-key-inhibit t))
	  :post (progn (setq which-key-inhibit nil)))
  ("LSP"
   (("p" lsp-ui-peek-find-definitions "peed")
    ("i" lsp-ui-imenu "imenu"))
   "Debug"
   (("r" quickrun "quickrun")
    ("s" quickrun-shell "quickrun shell")
    ("g" gdb "gdb"))
   ""
   (("t" toggle-debug-on-error)
    ("e" edebug-defun)
    ("l" edebug-remove-instrumentation)
    ("c" cancel-debug-on-entry)
    ("o" edebug-on-entry)
    ("C" edebug-cancel-on-entry)
    )))



(provide 'init-hydra)
;;; init-hydra.el ends here.
