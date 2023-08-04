;; lib-lyrics-fetcher.el --- Initialize lib-lyrics-fetcher configurations.  -*- lexical-binding: t; -*-

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

(defun lyrics-fetcher-neteasecloud-lyrics-get-time ()
  (when-let* ((timestamp (save-excursion
                           (beginning-of-line)
                           (re-search-forward "\\([[:digit:]]\\{2\\}\\):\\([[:digit:]]\\{2\\}\\)"
                                              (line-end-position)
                                              'noerror)))
              (minutes (string-to-number (match-string 1)))
              (seconds (string-to-number (match-string 2))))
    (+ (* 60 minutes) seconds)))

;;;###autoload
(defun lyrics-fetcher-neteasecloud-lyrics-jump ()
  "Seek the current player to the current timestamp."
  (declare (completion #'ignore))
  (interactive)
  (if-let ((time (lyrics-fetcher-neteasecloud-lyrics-get-time)))
      (progn
        (emms-seek-to time)
        (eli/lyrics-fetcher-hl-current-line))
    (message "No timestamp found!")))

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

(defun eli/lyrics-fetcher-hl-current-line ()
  (let* ((end (line-end-position))
         (beg (line-beginning-position))
         (ov (make-overlay beg end)))
    (remove-overlays (point-min) (point-max) 'lyrics-hl t)
    (overlay-put ov 'face 'mindre-keyword)
    (overlay-put ov 'lyrics-hl t)))

;;;###autoload
(defun eli/lyrics-fetcher-highlight (&rest _args)
  "Highlight the current lyrics."
  (when (eq major-mode 'lyrics-fetcher-view-mode)
    (save-excursion
      (let* ((m (/ (floor emms-playing-time) 60))
             (s (% (floor emms-playing-time) 60))
             (regexp (format "\\[%02d:%02d.*?\\]" m s)))
        (goto-char (point-min))
        (when (re-search-forward regexp (point-max) 'noerror)
          (eli/lyrics-fetcher-hl-current-line))))))

(defun eli/lyrics-fetcher-goto-current ()
  "Goto the position of the current lyrics when opening the lyrics file. "
  (let* ((lyrics (string-trim emms-lyrics-mode-line-string)))
    (if (search-forward lyrics (point-max) 'noerror)
        (eli/lyrics-fetcher-hl-current-line)
      (goto-char (point-min)))))


;;;; provide
(provide 'lib-lyrics-fetcher)
;;; lib-lyrics-fetcher.el ends here.
