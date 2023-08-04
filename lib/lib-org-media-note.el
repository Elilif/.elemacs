;; lib-org-media-note.el --- Initialize lib-org-media-note configurations.	-*- lexical-binding: t; -*-

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

(defvar org-media-note-edit-commands '(org-self-insert-command))

;;;###autoload
(defun eli/org-media-note-vedio-pause ()
  (interactive)
  (when mpv--process
    (mpv-pause)
    (letrec ((eli/auto-pause (lambda ()
                               (cond
                                ((not mpv--process)
                                 (remove-hook 'pre-command-hook eli/auto-pause t))
                                ((member this-command org-media-note-edit-commands)
                                 (mpv-pause)
                                 (remove-hook 'pre-command-hook eli/auto-pause t))))))
      (add-hook 'pre-command-hook eli/auto-pause nil t))))


;;;; provide
(provide 'lib-org-media-note)
;;; lib-org-media-note.el ends here.
