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

(cl-eval-when (compile)
  (require 'lyrics-fetcher))

(setup emms
  (:also-load
   lib-emms)
  (:option*
   emms-score-max-score 10
   emms-player-list '(emms-player-mpv)
   emms-lyrics-display-on-modeline t
   emms-lyrics-scroll-p nil
   emms-lyrics-display-on-minibuffer nil
   emms-lyrics-dir "~/Music/lyrics/"
   emms-source-file-default-directory "~/Music/"
   emms-playlist-buffer-name "*Emms*"
   emms-browser-covers #'emms-browser-cache-thumbnail-async
   emms-browser-thumbnail-medium-size 128
   emms-browser-thumbnail-small-size 64
   emms-player-mpv-update-metadata t))

(setup emms-info
  (:also-load
   emms-info-libtag)
  (:option*
   emms-info-functions '(emms-info-libtag)))

(setup lyrics-fetcher
  (:also-load
   lib-lyrics-fetcher)
  (:option*
   ;; use neteasecloud backend
   lyrics-fetcher-format-file-name-method #'eli/lyrics-fetcher-neteasecloud-format-file-name
   lyrics-fetcher-fetch-method #'lyrics-fetcher-neteasecloud-do-search
   lyrics-fetcher-format-song-name-method #'lyrics-fetcher-neteasecloud-format-song-name
   lyrics-fetcher-lyrics-file-extension ".lrc"
   emms-lyrics-dir lyrics-fetcher-lyrics-folder)
  (:bind-into lyrics-fetcher-view-mode-map
    "RET" lyrics-fetcher-neteasecloud-lyrics-jump)
  (:with-feature emms-browser
    (:bind-into emms-browser-mode-map
      "l" lyrics-fetcher-show-lyrics
      "L" lyrics-fetcher-emms-browser-show-at-point))
  (:with-feature emms
    (:advice
     emms-lyrics-display-handler :after eli/emms-lyrics-highlight))
  (:hooks
   lyrics-fetcher-view-mode-hook eli/emms-lyrics-sync))

(setup consult-emms
  (:option*
   consult-emms--sort-album-function #'string<))


;;;; provide
(provide 'init-music)
;;; init-music.el ends here.
