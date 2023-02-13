;; glean.el --- Initialize Args out of range: "glean.el", 23, 117 configurations.	-*- lexical-binding: t; -*-

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

(defgroup glean nil
  "")

(defcustom glean-path (expand-file-name "etc/glean/" user-emacs-directory)
  ""
  :group 'glean)

(defvar glean-types '(borg init))

(defun glean-delete-obsolete (file type)
  ;; cleanup obsolete autoloads file
  (dolist (f (directory-files glean-path t (concat
                                            (symbol-name type)
                                            "-[0-9]+-[0-9]+\\.elc?\\'")))
    (unless (string= file f)
      (delete-file f))))

(defun glean-generate-borg (file)
  (make-directory (file-name-directory file) 'parents)
  (message "Generating single big autoload file.")
  (glean-delete-obsolete file 'autoload)
  (condition-case-unless-debug e
      (with-temp-file file
        (setq-local coding-system-for-write 'utf-8)
        (let ((standard-output (current-buffer))
              (print-quoted t)
              (print-level nil)
              (print-length nil)
              (home (expand-file-name "~"))
              path-list
              theme-path-list
              drones-path
              auto)
          (insert ";; -*- lexical-binding: t; coding: utf-8; no-native-compile: t -*-\n"
                  ";; This file is generated from enabled drones.\n")

          ;; replace absolute path to ~
          (dolist (p load-path)
            ;; collect all drone's load-path
            (when (string-prefix-p (expand-file-name user-emacs-directory) (expand-file-name p))
              (push p drones-path))

            (if (string-prefix-p home p)
                (push (concat "~" (string-remove-prefix home p)) path-list)
              (push p path-list)))

          (dolist (p custom-theme-load-path)
            (if (and (stringp p)
                     (string-prefix-p home p))
                (push (concat "~" (string-remove-prefix home p)) theme-path-list)
              (push p theme-path-list)))

          (prin1 `(set `load-path ',(nreverse path-list)))
          (insert "\n")
          (print `(set `custom-theme-load-path ',(nreverse theme-path-list)))
          (insert "\n")

          ;; insert all drone's autoloads.el to this file
          (dolist (p drones-path)
            (when (file-exists-p p)
              (setq auto (car (directory-files p t ".*-autoloads.el\\'")))
              (when (and auto
                         (file-exists-p auto))
                (insert-file-contents auto))))
          ;; remove all #$ load code
          (goto-char (point-min))
          (while (re-search-forward "\(add-to-list 'load-path.*#$.*\n" nil t)
            (replace-match ""))

          ;; write local variables region
          (goto-char (point-max))
          (insert  "\n"
                   "\n;; Local Variables:"
                   "\n;; version-control: never"
                   "\n;; no-update-autoloads: t"
                   "\n;; End:"
                   ))
        t)
    (error (delete-file file)
           (signal 'collect-autoload-error (list file e)))))

(defun glean-generate-init (file)
  (make-directory (file-name-directory file) 'parents)
  (message "Generating single big init file.")
  (glean-delete-obsolete file 'init)
  (condition-case-unless-debug e
      (with-temp-file file
        (setq-local coding-system-for-write 'utf-8)
        (let* ((dir (concat user-emacs-directory "lisp/"))
               (default-directory dir)
               (files (directory-files dir nil "^\\([^#.~]\\)")))
          (dolist (file files)
            (insert-file-contents file))
          (goto-char (point-max))
          (save-excursion
            (while (re-search-backward "(provide.*\n.*ends here." nil t)
              (delete-region (match-beginning 0)
                             (match-end 0))))
          (insert (format "(provide '%s)" (file-name-base file))))
        )
    (error (delete-file file)
           (signal 'collect-init-error (list file e)))))

(defun glean-generate-core (file)
  (make-directory (file-name-directory file) 'parents)
  (message "Generating single big init file.")
  (glean-delete-obsolete file 'core)
  (condition-case-unless-debug e
      (with-temp-file file
        (setq-local coding-system-for-write 'utf-8)
        (let* ((dir (concat user-emacs-directory "core/"))
               (default-directory dir)
               (files (directory-files dir nil "^\\([^#.~]\\)")))
          (dolist (file files)
            (insert-file-contents file))
          (goto-char (point-max))
          (save-excursion
            (while (re-search-backward "(provide.*\n.*ends here." nil t)
              (delete-region (match-beginning 0)
                             (match-end 0))))
          (insert (format "(provide '%s)" (file-name-base file))))
        )
    (error (delete-file file)
           (signal 'collect-init-error (list file e)))))

(defun glean-genere-file (base monitor)
  (concat glean-path
          (symbol-name base)
          "-"
          (format-time-string
           "%+4Y%m%d-%H%M%S"
           (file-attribute-modification-time
            (file-attributes monitor)))
          ".el"))

(defun glean-generate (type)
  (pcase type
    ('borg
     (let ((file (glean-genere-file 'autoload "/home/eli/src/Cradle/elemacs/.gitmodules")))
       (if (file-exists-p file)
           (load file nil t)
         (require 'borg)
         (borg-initialize)
         (glean-generate-borg file))))
    ('init
     (let ((file (glean-genere-file 'init (concat user-emacs-directory "lisp"))))
       (if (file-exists-p file)
           (load file nil t)
         (glean-generate-init file)
         (load file nil t))))
    ('core
     (let ((file (glean-genere-file 'core (concat user-emacs-directory "core"))))
       (if (file-exists-p file)
           (load file nil t)
         (glean-generate-core file)
         (load file nil t))))))



(provide 'glean)
;;; glean.el ends here.
