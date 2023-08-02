;; lib-emms.el --- Initialize lib-emms configurations.	-*- lexical-binding: t; -*-

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


;; check whether a musical file is lossless.
(defun eli/sox-spectrogram (file)
  "Show spectrogram for FILE."
  (interactive (list (read-file-name "Select a file: " "~/Music/")))
  (let* ((output (concat (make-temp-name "/tmp/sox-")
                         ".png"))
         (command (format
                   "sox %s -n spectrogram -o %s"
                   (shell-quote-argument file)
                   output)))
    (if (= (shell-command command) 0)
        (find-file output)
      "Something wrong!")))

;; align track numbers
(defun eli/emms-browser-track-number (track)
  "Return a string representation of a track number.
The string will end in a space. If no track number is available,
return an empty string."
  (let ((tracknum (emms-track-get track 'info-tracknumber)))
    (cond
     ((or (not (stringp tracknum)) (string= tracknum "0"))
      "")
     ((or (eq (length tracknum) 1)
          (string-match-p "^[[:digit:]]/[[:digit:]]\\{2,\\}" tracknum))
      (concat "0" tracknum))
     ((string-match-p "^[[:digit:]]/[[:digit:]]" tracknum)
      (concat
       "0"
       (substring tracknum 0 1)
       "/0"
       (substring tracknum 2 3)))
     (t
      tracknum))))

(defun eli/emms-echo-current ()
  "Display the currently playing song in the echo area."
  (interactive)
  (message (emms-track-description
            (emms-playlist-current-selected-track))))

;;;; provide
(provide 'lib-emms)
;;; lib-emms.el ends here.
