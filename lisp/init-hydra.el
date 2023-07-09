;; init-hydra.el --- Initialize hydra configurations.	-*- lexical-binding: t -*-

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
(keymap-global-set "C-c h" #'hydra-gpt/body)
(keymap-global-set "C-c p" #'hydra-puni/body)
(keymap-global-set "s-r" #'hydra-rectangle/body)
(keymap-global-set "C-c [" #'hydra-skan-user-buffers-prev/body)
(keymap-global-set "C-c ]" #'hydra-skan-user-buffers-next/body)
(keymap-global-set "C-c n" #'hydra-org-noter/body)
(keymap-global-set "C-c a" #'hydra-org-agenda/body)
(keymap-global-set "M-[" #'org-media-note-hydra/body)

(pretty-hydra-define hydra-search
  (:color amaranth :exit t :quit-key "q"
	      :pre (progn (setq which-key-inhibit t))
	      :post (progn (setq which-key-inhibit nil) ))
  ("Dicts"
   (("t" eli/te-search "search TE")
    ("d" eli/dict-search "search Dicts"))
   "Deft"
   (("n" notdeft "notdeft"))
   "info"
   (("i" eli/info-search))
   "rg"
   (("g" eli/consult-git-grep)
    ("r" eli/consult-git-ripgrep)
	("f" consult-find)
    ("e" (eli/consult-git-ripgrep "~/.emacs.d/site-lisp/") "lib"))
   "Google"
   (("s" my-search-with-chrome)
    ("c" github-copy-reference-url-at-point))))

(pretty-hydra-define hydra-gpt
  (:color amaranth :exit t :quit-key "q")
  (""
   (("k" eli/gptel-exit)
	("b" eli/gptel-translate)
	("c" eli/gptel-translate-cc)
	("s" eli/gptel-summary))
   ""
   (("r" eli/gptel-translate-and-insert)
	("e" eli/gptel-program)
	("p" eli/gptel-polish)
	("w" eli/gptel-translate-word)
	("g" eli/gptel-grammar-correct))))

(pretty-hydra-define hydra-edit
  (:color amaranth :exit t
	      :pre (progn (setq which-key-inhibit t))
	      :post (progn (setq which-key-inhibit nil)))
  ("Format"
   (("f" smart-align "align"))
   "Beacon"
   (("w" markmacro-mark-words "mark words")
    ("l" markmacro-mark-lines "mark lines")
    ("k" rectangle-mark-mode "mark rectangle"))
   ""
   (("g" markmacro-secondary-region-set)
    ("j" markmacro-secondary-region-mark-cursors)
    ("s" markmacro-swap-region))
   ))

(pretty-hydra-define hydra-roam
  (:color amaranth :exit t :quit-key "q"
	      :pre (progn (setq which-key-inhibit t))
	      :post (progn (setq which-key-inhibit nil) ))
  ("Roam"
   (("l" org-roam-buffer-toggle "toggle roam buffer")
    ("f" eli/org-roam-node-find "find roam node")
    ("n" org-id-get-create "create roam id")
    ("i" (org-roam-node-insert #'eli/org-roam-filter-books) "insert roam node")
    ("s" consult-org-roam-headline "search headings")
    ("w" org-roam-refile "refile roam node"))
   "Roam"
   (("c" org-roam-dailies-capture-today "roam capture")
    ("ra" org-roam-ref-add "add refs")
    ("rd" org-roam-ref-remove "remove a ref")
    ("t" org-roam-tag-add "add tags")
    ("v" org-roam-tag-remove "remove a tag"))
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
  (:color amaranth :exit t :quit-key "q"
	      :pre (progn (setq which-key-inhibit t))
	      :post (progn (setq which-key-inhibit nil) ))
  ("basic"
   (("E" eval-buffer)
    ("f" (let ((default-directory "~/.emacs.d/lisp/"))
           (call-interactively 'find-file)) "config files")
	("l" eli/setup-open-lib)
    ("R" restart-emacs)
    ("c" (byte-recompile-directory "~/.emacs.d/lisp/") "recompile"))
   "Bookmark"
   (("bs" bookmark-set "set bookmark")
    ("bj" consult-bookmark "jump bookmark")
    ("bd" bookmark-delete "delete bookmark"))
   "basic"
   (("s" scratch-buffer))
   ))

(pretty-hydra-define hydra-puni
  (:color amaranth :exit t
		  :quit-key "q" :idle 2)
  (""
   (("k" puni-squeeze)
	("r" puni-raise)
	("c" puni-convolute)
	("p" puni-split)
	("s" puni-splice))
   "slurp & barf"
   (("lf" puni-slurp-forward)
	("lb" puni-slurp-backward)
	("bf" puni-barf-forward)
	("bb" puni-barf-backward))))

(pretty-hydra-define hydra-org
  (:color amaranth :exit t :quit-key "q")
  ("Basic"
   (("h" org-mode "org mode")
    ("j" consult-org-heading "headings")
    ("f" eli/consult-org-file "agenda filter")
	("m" org-heatmap-calendar "heatmap"))
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
    ("ar" org-anki-sync-region "region")
    ("s" eli/org-show-empty-headline "show empty headline"))
   "blog"
   (("bp" org-publish)
    ("ba" (org-publish "eli's blog" t) "publish all blogs")
    ("bu" (progn
            (org-publish "eli's blog")
            (org-publish "eli's blog rss")) "update rss and blog")
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

(with-eval-after-load 'org-media-note
  (pretty-hydra-define org-media-note-hydra
    (:color red
            :title (org-media-note--hydra-title)
            :hint nil)
    ("File"
     (("o" org-media-note-mpv-smart-play
       (if (org-media-note-ref-cite-p)
           (format "Open %s"
                   (org-media-note--current-org-ref-key))
         "Open file")
       :width 20)
      ("j"
       (mpv-cycle-property "sub")
       "toggle subtitles")
      ("T"
       (mpv-cycle-property "ontop")
       "toggle ontop")
      ("c"
       (org-media-note-change-speed-by 0.1)
       "increase speed")
      ("x"
       (org-media-note-change-speed-by -0.1)
       "decrease speed")
      ("z" org-media-note-mpv-toggle-speed "reset speed"))
     "Playback"
     (("<SPC>" mpv-pause "Play/Pause" :exit t)
      ("p" mpv-pause "Play/Pause")
      ("l"
       (mpv-run-command "ab-loop")
       (let ((time-a (mpv-get-property "ab-loop-a"))
             (time-b (mpv-get-property "ab-loop-b")))
         (if (org-media-note--ab-loop-p)
             (format "Clear A-B loop (%s - %s)"
                     (org-media-note--seconds-to-timestamp time-a)
                     (org-media-note--seconds-to-timestamp time-b))
           (if (numberp time-a)
               (format "Set B of A-B loop (%s - )"
                       (org-media-note--seconds-to-timestamp time-a))
             "Set A of A-B loop")))
       :width 45)
      ("g" org-media-note-goto-timestamp "Jump to the timestamp")
      ("b" mpv-seek-backward "Back 5s")
      ("f" mpv-seek-forward "Forward 5s")
      ("C-b"
       (mpv-run-command "sub-seek" -1)
       "Previous subtitle")
      ("C-f"
       (mpv-run-command "sub-seek" 1)
       "Next subtitle"))
     "Volume"
     (("+"
       (org-media-note-change-volume-by 5)
       "Up")
      ("-"
       (org-media-note-change-volume-by -5)
       "Down")
      ("0" org-media-note-mpv-toggle-volume "toggle")
      ("m"
       (mpv-cycle-property "mute")
       "(un)mute"))
     "Note"
     (("i" org-media-note-insert-link "Insert timestamp")
      ("a" org-media-note-adjust-timestamp-offset "Adjust timestamp")
      ("S" org-media-note-insert-screenshot "Insert Screenshot")
      ("s" org-media-note-insert-sub-text "Insert subtitle"))
     "Import"
     (("I p" org-media-note-insert-note-from-pbf
       "Import from pbf")
      ("I n" org-media-note-insert-note-from-noted
       "Import from Noted")
      ("I t" org-media-note-convert-from-org-timer
       "Import from org-timer")
      ("I s" org-media-note-insert-note-from-srt
       "Import from srt"))
     "Toggle"
     (("t m" org-media-note-mode "Auto insert media item"
       :toggle t)
      ("t c" org-media-note-toggle-refcite "Use ref key instead of absolute path"
       :toggle org-media-note-use-refcite-first)
      ("t p" org-media-note-toggle-pause-after-insertion
       "Pause media after insert link" :toggle org-media-note-pause-after-insert-link)
      ("t s" org-media-note-toggle-save-screenshot
       "Auto save screenshot" :toggle org-media-note-save-screenshot-p)
      ("t S" org-media-note-toggle-screenshot-with-sub
       "Screenshot with subtitles" :toggle org-media-note-screenshot-with-sub)
      ("t t" org-media-note-toggle-timestamp-pattern
       (format "Timestamp format: %s"
               (cond
                ((eq org-media-note-timestamp-pattern 'hms)
                 "hh:mm:ss")
                ((eq org-media-note-timestamp-pattern 'hmsf)
                 "hh:mm:ss.fff"))))))))

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
  (:color amaranth :exit t
	      :pre (progn (setq which-key-inhibit t))
	      :post (progn (setq which-key-inhibit nil)))
  ("Input"
   (("s" eli/input-switch "switch input")
    ("k" eli/insert-key-sequence "insert key sequence")
	("w" whisper-run)
	("t" eli/org-table-create "matrices"))
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
  (:color amaranth :exit t
	      :pre (progn (setq which-key-inhibit t))
	      :post (progn (setq which-key-inhibit nil)))
  ("LSP"
   (("p" lsp-ui-peek-find-definitions "peek")
    ("i" lsp-ui-imenu "imenu"))
   "Debug"
   (("r" quickrun "quickrun")
    ("s" quickrun-shell "quickrun shell")
    ("g" gdb "gdb")
    ("c" compile)
	("a" eli/helpful-remove-advice-i)
	("h" remove-hook))
   ""
   (("t" toggle-debug-on-error)
    ("e" edebug-defun)
    ("l" edebug-remove-instrumentation)
    ("n" cancel-debug-on-entry)
    ("o" edebug-on-entry)
    ("N" edebug-cancel-on-entry))
   "leetcode"
   (("k" eli/leetcode-kill-problems-buffer)
    ("d" leetcode))))

(pretty-hydra-define hydra-rectangle
  (:color amaranth :exit t
	      :pre (progn (setq which-key-inhibit t))
	      :post (progn (setq which-key-inhibit nil)))
  ("rectangle"
   (("k" kill-rectangle)
    ("w" copy-rectangle-as-kill)
    ("d" delete-rectangle)
    ("y" yank-rectangle)
    ("SPC" rectangle-mark-mode))
   ""
   (("o" open-rectangle)
    ("n" rectangle-number-lines)
    ("c" clear-rectangle)
    ("l" delete-whitespace-rectangle)
    ("i" string-rectangle)
    ("s" string-insert-rectangle))))


(provide 'init-hydra)
;;; init-hydra.el ends here.
