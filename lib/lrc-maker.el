;;; lrc-maker.el --- generate lrc fils -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

;; Version: 0.1
;; Package-Requires: ((emms "15") (lyrics-fetcher))
;; Keywords: music, lrycis
;; SPDX-License-Identifier: GPL-3.0-or-later


(require 'emms)
(require 'lyrics-fetcher)

(defvar lrc-maker-timer-start-time nil)
(defvar lrc-maker-timer-pause-time nil)

;;;###autoload
(defun lrc-maker-timer-start ()
  "Set the starting time for the relative timer to now."
  (interactive)
  (setq lrc-maker-timer-start-time (current-time)
        lrc-maker-timer-pause-time nil)
  (emms-stop)
  (emms-start))

;;;###autoload
(defun lrc-maker-timer-stop ()
  "Stop the relative timer."
  (interactive)
  (unless lrc-maker-timer-start-time
    (user-error "No timer running"))
  (setq lrc-maker-timer-start-time nil
	    lrc-maker-timer-pause-time nil)
  (message "Timer stopped"))

(defun lrc-maker-generate-timestamp ()
  "Generate the timesamp used by `.lrc' files."
  (let* ((start-secs (float-time lrc-maker-timer-start-time))
         (pause-secs (float-time lrc-maker-timer-pause-time))
         (duration (- pause-secs start-secs))
         (sec (floor duration))
         (ms (substring (number-to-string (- duration sec)) 2 4))
         (m (/ sec 60)))
    (setq sec (% sec 60))
    (format "[%02d:%02d.%s]" m sec ms)))

;;;###autoload
(defun lrc-maker-insert ()
  "Insert the current timestamp."
  (interactive)
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (when (looking-at-p "\\[.*\\]")
      (kill-sexp))
    (insert (lrc-maker-generate-timestamp))
    (forward-line)))

;;;###autoload
(defun lrc-marker-undo ()
  "Undo some previous changes."
  (interactive)
  (let ((inhibit-read-only t))
    (undo)))
;;;###autoload
(defun lrc-maker-delete ()
  "Delete the current timestamp."
  (interactive)
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (when (looking-at-p "\\[.*\\]")
      (kill-sexp))))

;;;###autoload
(defun lrc-maker-timer-pause ()
  "Pause the relative timer."
  (interactive)
  (cond
   (lrc-maker-timer-pause-time
    (let ((start-secs (float-time lrc-maker-timer-start-time))
          (pause-secs (float-time lrc-maker-timer-pause-time)))
      (setq lrc-maker-timer-start-time (time-since (- pause-secs start-secs))
            lrc-maker-timer-pause-time nil)))
   (t
    (setq lrc-maker-timer-pause-time (float-time (current-time)))))
  (emms-pause))

;;;###autoload
(defun lrc-maker-seek-forward ()
  "Seek `emms-seek-seconds' forward."
  (interactive)
  (setq lrc-maker-timer-start-time
        (time-since (+ (float-time emms-seek-seconds)
                       (- (float-time (current-time))
                          (float-time lrc-maker-timer-start-time)))))
  (emms-seek-forward))

;;;###autoload
(defun lrc-maker-seek-backward ()
  "Seek `emms-seek-seconds' backward."
  (interactive)
  (let ((elapse (- (float-time (current-time))
                   (float-time lrc-maker-timer-start-time)
                   (float-time emms-seek-seconds))))
    (setq lrc-maker-timer-start-time (time-since
                                      (if (< elapse 0)
                                          0
                                        elapse))))
  (emms-seek-backward))

;;;###autoload
(defun lrc-maker-seek-next (&optional arg)
  (interactive "P")
  (forward-line (or arg 1))
  (lyrics-fetcher-neteasecloud-lyrics-jump)
  (when-let ((time (lyrics-fetcher-neteasecloud-lyrics-get-time)))
    (setq lrc-maker-timer-start-time (time-since time))))

;;;###autoload
(defun lrc-maker-seek-previous (&optional arg)
  (interactive "P")
  (lrc-maker-seek-next (- (or arg 1))))

;;;###autoload
(defun lrc-maker-timer-set ()
  "Set the relative time when toggle `lyrics-fetcher-view-mode-hook'."
  (when emms-player-playing-p
    (lrc-maker-mode)
    (setq lrc-maker-timer-start-time (time-since
                                      (float-time emms-playing-time)))))
;;;###autoload
(defun lrc-maker-lyrics-get ()
  "Get the orignal lyrics form Genius."
  (interactive)
  (let ((lyrics-fetcher-fetch-method #'lyrics-fetcher-genius-do-search)
        (lyrics-fetcher-format-file-name-method #'lyrics-fetcher-format-file-name)
        (lyrics-fetcher-format-song-name-method #'lyrics-fetcher-format-song-name)
        (lyrics-fetcher-lyrics-file-extension ".lrc"))
    (lyrics-fetcher-show-lyrics)))

;;;###autoload
(defun lrc-maker-save ()
  "Save the current lyrics."
  (interactive)
  (let* ((track (emms-playlist-current-selected-track))
         (file-name (funcall lyrics-fetcher-format-file-name-method track)))
    (unless (file-exists-p lyrics-fetcher-lyrics-folder)
	  (make-directory lyrics-fetcher-lyrics-folder))
    (write-file (lyrics-fetcher--process-filename file-name))))

;;;###autoload
(define-minor-mode lrc-maker-mode
  "Toggle `lrc-maker-mode'."
  :global nil
  :keymap
  (define-keymap
    "i" #'lrc-maker-insert
    "SPC" #'lrc-maker-timer-pause
    "s" #'lrc-maker-timer-stop
    "r" #'lrc-maker-timer-start
    "b" #'lrc-maker-seek-backward
    "d" #'lrc-maker-delete
    "f" #'lrc-maker-seek-forward
    "u" #'lrc-marker-undo
    "C-x C-s" #'lrc-maker-save
    "n" #'lrc-maker-seek-next
    "p" #'lrc-maker-seek-previous)
  (cond
   (lrc-maker-mode
    (read-only-mode 1))
   (t
    (read-only-mode -1)
    (lrc-maker-timer-stop))))

(provide 'lrc-maker)
;;; lrc-maker.el ends here
