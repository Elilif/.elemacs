;; lib-global-interactive.el --- Initialize global-interactive configurations.	-*- lexical-binding: t; -*-

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

;;; enable call emacs functions from outside
;;; from https://isamert.net/2022/03/16/global-interactive-emacs-functions.html

(defvar elemacs-global-interactive-defer-to-system-app nil)

(defvar elemacs-global-interactive-commands nil)


(defun elemacs-global-interactive-system-read-string (prompt)
  "Like `read-string' but use an Emacs independent system level app
to get user input. You need to install `zenity'."
  (string-trim
   (shell-command-to-string
    (format "zenity --entry --text='%s'" prompt))))

(advice-add #'read-string :around (lambda (orig-fun prompt &rest args)
                                    (if elemacs-global-interactive-defer-to-system-app
                                        (elemacs-global-interactive-system-read-string prompt)
                                      (apply orig-fun prompt args))))

(defun elemacs-global-interactive-dmenu (prompt items &rest ignored)
  "Like `completing-read' but instead use dmenu.
Useful for system-wide scripts."
  (with-temp-buffer
    (thread-first
      (cond
       ((functionp items)
        (funcall items "" nil t))
       ((listp (car items))
        (mapcar #'car items))
       (t
        items))
      (string-join "\n")
      string-trim
      insert)
    (shell-command-on-region
     (point-min)
     (point-max)
     (pcase system-type
       ('gnu/linux (format "rofi -dmenu -fuzzy -i -p '%s' -theme $HOME/.config/rofi/carbonized/config.rasi" prompt))
       ('darwin "choose"))
     nil t "*elemacs-global-interactive-dmenu error*" nil)
    (string-trim (buffer-string))))

(defun elemacs-global-interactive-run ()
  "enable selecting a functions from `elemacs-global-interactive-commands' and call it."
  (let*
      ((completing-read-function #'elemacs-global-interactive-dmenu)
       (elemacs-global-interactive-defer-to-system-app t)
       (candidates (mapcar #'symbol-name  elemacs-global-interactive-commands))
       (selected-item (completing-read "Select: " candidates)))
    (unless (string-empty-p selected-item)
      (funcall (intern selected-item)))))


;;; support call org-capture from outside.
(defvar elemacs-global-interactive-capture-p nil
  "Non-nil if call `org-capture' through `elemacs-global-interactive--capture'.")

(defun elemacs-global-interactive-capture ()
  "For use as an `org-capture-templates-contexts'."
  elemacs-global-interactive-capture-p)

(defun elemacs-global-interactive--capture ()
  "Call `org-capture' from outside."
  (let* ((key-alist '(("Note" . "1")
                      ("TODO" . "2")))
         (key (cdr (assoc (completing-read "Input: " '("Note" "TODO")) key-alist)))
         (elemacs-global-interactive-capture-p t)
         (org-capture-initial (when key
                                (completing-read "Input: " nil))))
    (when (and key (not (string-empty-p org-capture-initial)))
      (org-capture nil key))))



(provide 'lib-global-interactive)
;;; lib-global-interactive.el ends here.
