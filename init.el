;; init.el      -*- lexical-binding: t -*-

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





;;;; Load Path
(add-to-list 'load-path (concat user-emacs-directory "core"))
(add-to-list 'load-path (concat user-emacs-directory "lib"))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
;;;; Benchmark
(require 'core-benchmarking)

;;;; Borg
;; (eval-and-compile ; `borg'
;;   (add-to-list 'load-path (expand-file-name "site-lisp/borg" user-emacs-directory))
;;   ;; (add-to-list 'load-path "~/.emacs.d/lib/borg")
;;   (require 'borg)
;;   (borg-initialize))

(eval-and-compile ; `borg'
  (defvar single-autoload-path (expand-file-name "etc/borg/autoload/" user-emacs-directory)
    "single autoload file.")

  (add-to-list 'load-path (expand-file-name "site-lisp/borg" user-emacs-directory))

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

            (prin1 `(set `load-path ',path-list))
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

  (defun lld-initialize ()
    (let ((file (concat single-autoload-path
                        "autoload-"
                        (format-time-string
                         "%+4Y%m%d-%H%M%S"
                         (file-attribute-modification-time
                          (file-attributes "~/.emacs.d/.gitmodules")))
                        ".el")))
      (if (and (file-exists-p file)
               initial-window-system)
          (load file nil t)
        (require 'borg)
        (borg-initialize)
        (lld-collect-autoloads file))))

  (defun latest-file (path)
    "Get latest file (including directory) in PATH."
    (file-name-nondirectory (car (seq-find
                                  (lambda (x) (not (nth 1 x))) ; non-directory
                                  (sort
                                   (directory-files-and-attributes path 'full nil t)
                                   (lambda (x y) (time-less-p (nth 5 y) (nth 5 x))))))))

  (defun eli/collect-lib-autoloads ()
    (unless (string= (latest-file "~/.emacs.d/lib/") "lib-autoloads.el")
      (require 'loaddefs-gen nil t)
      (loaddefs-generate "~/.emacs.d/lib/"
                         "~/.emacs.d/lib/lib-autoloads.el"
                         nil nil nil t))
    (load "~/.emacs.d/lib/lib-autoloads.el" nil t))

  (lld-initialize)
  (eli/collect-lib-autoloads))


;;;; Core
(let ((file-name-handler-alist nil))
  (require 'core-lib)
  (require 'core-incremental-loading)
  (require 'core-setup)
  (require 'core-ui)
  (require 'core-better-default)

  
;;;; Modules
  (require 'init-completion)
  (run-with-idle-timer 0.3 nil (lambda () (require 'init-hydra)))
  (require 'init-misc)
  (require 'init-info)
  (require 'init-edit))

;; (require 'init-lang)
;; (require 'init-news)
;; (require 'init-org)
;; (require 'init-pdf)
;; (require 'init-bib)

;;; HACK: load the following config on demand, need more tests.
;;; TODO: check the entire loading procedure and refactor it.
(setup org
  (:iload init-org)
  (:once (list :before 'hydra-org-agenda/body 'hydra-org/body
               'citar-open
               :packages 'org)
    (require 'init-org)
    (require 'init-blog)
    (require 'init-tex)
    (load "~/.emacs.d/custom.el")))

(setup init-vc
  (:iload init-vc)
  (:once (list :before 'magit-status)
    (require 'init-vc)))

(setup init-lang
  (:iload init-lang)
  (:once (list
          :hooks 'find-file-hook
          :before 'consult-recent-file 'consult-dir 'consult-bookmark)
    (require 'init-lang)))

(setup init-news
  (:iload init-news)
  (once (list :before 'hydra-reader/body)
    (require 'init-news)))

(setup reader
  (:iload init-pdf init-bib)
  (:once (list :before 'hydra-bibtex/body)
    (require 'init-pdf)
    (require 'init-bib)))

(setup init-music
  (:once (list :before 'hydra-player/body 'eli/pop-to-buffer)
    (require 'init-music)
    (emms-all)
    (emms-history-load)
    (emms-mode-line-disable))
  (:global
   "C-\\" eli/pop-to-buffer))

;;; init.el ends here.
