;; init.el -*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 by Eli

;; Author: Eli <eli.q.qian@gmail.com>
;; URL: https://github.com/Elilif/.emacs.d

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

(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))

(setq user-emacs-directory "~/.emacs.d/")
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-benchmarking)

;; (eval-and-compile ; `borg'
;;   (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
;;   (require 'borg)
;;   (borg-initialize))
(eval-and-compile ; `borg'

  (defun elemacs/borg-clean (clone)
    (let* ((path (borg--expand-load-path clone nil))
           (file (expand-file-name (format "%s-autoloads.el" clone) (car path))))
	  (delete-file file)))
  (advice-add 'borg-clean :after #'elemacs/borg-clean)
  
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (setq borg-compile-function #'borg-byte+native-compile-async)

  (defun lld-collect-autoloads (file)
    "insert all enabled drone's autoloads file to a single file."
    (make-directory (file-name-directory file) 'parents)

    ;; cleanup obsolete autoloads file
    (dolist (f (directory-files single-autoload-path t "autoload-[0-9]+-[0-9]+\\.elc?\\'"))
      (unless (string= file f)
        (delete-file f)))

    (message "Generating single big autoload file.")
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

  (defvar single-autoload-path "~/.emacs.d/etc/borg/autoload/" "single autoload file.")
  (let ((file (concat single-autoload-path
                      "autoload-"
                      (format-time-string
                       "%+4Y%m%d-%H%M%S"
                       (file-attribute-modification-time
                        (file-attributes "/home/eli/.emacs.d/.gitmodules")))
                      ".el")))
    (if (file-exists-p file)
        (load file nil t)
      (require 'borg)
      (borg-initialize)
      (lld-collect-autoloads file))))

(let ((gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))  
  (require 'init-incremental-loading)
  (require 'init-better-defaults)
  (require 'init-ui)
  (require 'init-minibuffer)
  (require 'init-corfu)
  (require 'init-org)
  (require 'init-bib)
  (require 'init-latex)
  (require 'init-vc)
  (require 'init-lang)
  (require 'init-c)
  (require 'init-lsp)
  (require 'init-r)
  (require 'init-completion)
  (require 'init-flycheck)
  (require 'init-reader)
  (require 'init-news)
  (require 'init-spell)
  (require 'init-blog)
  (require 'init-music)
  (require 'init-finance)
  )

;; fix keymap-set completing error
(defun keymap-set--anon-cmacro
    (form keymap &optional key definition)
  (ignore keymap key definition)
  (keymap--compile-check key)
  form)

(defun eli/open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(keymap-global-set "<f5>" 'eli/open-init-file)

(provide 'init)
;;; init.el ends here.
