;; lib-lyrics-fetcher.el --- Initialize lib-lyrics-fetcher configurations.	-*- lexical-binding: t; -*-

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

;;;###autoload
(defun lyrics-fetcher-neteasecloud-lyrics-jump ()
  "Seek the current player to the current timestamp."
  (declare (completion #'ignore))
  (interactive)
  (let* ((timestamp (save-excursion
			          (beginning-of-line)
			          (thing-at-point 'sexp ':no-properties)))
	     (minutes (string-to-number
			       (progn
			         (string-match "\\([[:digit:]]\\{2\\}\\):\\([[:digit:]]\\{2\\}\\)" timestamp)
			         (match-string 1 timestamp))))
	     (seconds (string-to-number
			       (match-string 2 timestamp))))
	(if timestamp
	    (emms-seek-to (+ (* 60 minutes) seconds))
	  (message "No timestamp found!"))))

(defun eli/lyrics-fetcher-neteasecloud-format-file-name (track)
  "TRACK should be either a string or EMMS alist.
'Emms' requires lyrics files' name should be the same as their
tracks' name except extensions."
  (if (stringp track)
      (substring
       (lyrics-fetcher--prepare-string track)
       0
       (min (length track) 250))
    (let* ((album (emms-track-get track 'info-album))
           (dir (concat lyrics-fetcher-lyrics-folder album "/"))
           (full-name (if (not album)
                          (emms-track-get track 'name)
                        (unless (and (file-exists-p dir)
		                             (directory-name-p dir))
	                      (make-directory dir t))
                        (concat album
                                "/"
                                (file-name-nondirectory
                                 (emms-track-get track 'name))))))
      (emms-replace-regexp-in-string
       (concat "\\." (file-name-extension full-name) "\\'")
       ""
       full-name))))

(defvar eli/emms-lyrics-current-lyrics nil)

(defun eli/emms-lyrics-highlight (_lyric _next-lyric line _diff &optional hookp)
  "Highlight the current lyrics."
  (unless hookp
    (setq eli/emms-lyrics-current-lyrics line))
  (when (eq major-mode 'lyrics-fetcher-view-mode)
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- (or line 1)))
      (let* ((end (line-end-position))
             (beg (line-beginning-position))
             (ov (make-overlay beg end)))
        (remove-overlays (point-min) (point-max) 'lyrics-hl t)
        (overlay-put ov 'face 'mindre-keyword)
        (overlay-put ov 'lyrics-hl t)))))

(defun eli/emms-lyrics-sync ()
  (when (eq this-command 'lyrics-fetcher-show-lyrics)
    (goto-char (point-min))
    (forward-line (1- (or eli/emms-lyrics-current-lyrics 1)))
    (eli/emms-lyrics-highlight nil nil eli/emms-lyrics-current-lyrics nil t)))

;;;; provide
(provide 'lib-lyrics-fetcher)
;;; lib-lyrics-fetcher.el ends here.
