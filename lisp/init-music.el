;; init-music.el --- Initialize musical configurations.	-*- lexical-binding: t -*-

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
  (interactive (list (let ((default-directory "~/Music/"))
                       (read-file-name "Select a file: "))))
  (let ((output (concat (make-temp-name "/tmp/sox-")
                        ".png")))
    (shell-command (format
                    "sox %s -n spectrogram -o %s"
                    (shell-quote-argument file)
                    output))
    (find-file output)))

(setq )
(setq )

(setup emms
  (:also-load
   lib-emms)
  (:option*
   emms-score-max-score 10
   emms-player-list '(emms-player-mpv)
   emms-lyrics-display-on-modeline nil
   emms-lyrics-display-on-minibuffer t
   emms-lyrics-display-on-minibuffer t
   emms-lyrics-dir "~/Music/lyrics"
   emms-source-file-default-directory "~/Music/"
   emms-playlist-buffer-name "*Emms*"
   emms-browser-covers #'emms-browser-cache-thumbnail-async
   emms-browser-thumbnail-medium-size 128
   emms-browser-thumbnail-small-size 64))


;;;; provide
(provide 'init-music)
;;; init-music.el ends here.
