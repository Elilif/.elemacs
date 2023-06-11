;; lib-leetcode.el --- Initialize lib-leetcode configurations.	-*- lexical-binding: t; -*-

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

;;;;; leetcode lib

;;;###autoload
(defun eli/leetcode-kill-problems-buffer ()
  "Close and delete leetcode related buffers and windows."
  (interactive)
  (mapc (lambda (title)
          (leetcode--kill-buff-and-delete-window
           (get-buffer (leetcode--get-code-buffer-name title)))
          (let* ((slug-title (leetcode--slugify-title title))
                 (problem (leetcode--get-problem slug-title))
                 (problem-id (leetcode-problem-id problem)))
            (leetcode--kill-buff-and-delete-window (get-buffer (leetcode--detail-buffer-name problem-id)))
            (leetcode--kill-buff-and-delete-window (get-buffer (leetcode--result-buffer-name problem-id)))
            (leetcode--kill-buff-and-delete-window (get-buffer (leetcode--testcase-buffer-name problem-id)))))
        leetcode--problem-titles)
  (setq leetcode--problem-titles nil))

(defvar leetcode-lang-class-keyword '(("cpp" . "class"))
  "Alist of (LANG . CLASS KEYWORD) pairs.

  This alist will be  parsed by `eli/leetcode--buffer-content'.")

(defun eli/leetcode--buffer-content (buf)
  "Get content without text properties of BUF."
  (with-current-buffer buf
    (let* ((string (alist-get leetcode--lang
                              leetcode-lang-class-keyword
                              "class" nil #'string=))
           (point (save-excursion
                    (goto-char (point-max))
                    (re-search-backward (concat "^" string) nil t)
                    (line-beginning-position))))
      (buffer-substring-no-properties
       (if (buffer-file-name buf)
           point
         (point-min))
       (point-max)))))



;;;; provide
(provide 'lib-leetcode)
;;; lib-leetcode.el ends here.
