;; lib-gdb.el --- Initialize lib-gdb configurations.	-*- lexical-binding: t; -*-

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
  (require 'gdb-mi))

(defun gdb-non-stop-handler ()
  (goto-char (point-min))
  (if (re-search-forward "No symbol" nil t)
      (progn
	    (message
         "This version of GDB doesn't support non-stop mode.  Turning it off.")
	    (setq gdb-non-stop nil)
	    (setq gdb-supports-non-stop nil))
    (setq gdb-supports-non-stop t)
    (gdb-input "-gdb-set mi-async 1" 'ignore)
    (gdb-input "-list-target-features" 'gdb-check-target-async)))

(defun eli/reset-gud-gdb-history (&rest _args)
  (when (eq this-command 'gdb)
    (let* ((out-file (file-name-nondirectory
                      (file-name-with-extension (buffer-file-name)
                                                "out")))
           (gdb-cmdline (concat gud-gdb-command-name
                                " "
                                out-file)))
      (unless (string= gdb-cmdline (car gud-gdb-history))
        (push gdb-cmdline gud-gdb-history)))))


;;;; provide
(provide 'lib-gdb)
;;; lib-gdb.el ends here.
