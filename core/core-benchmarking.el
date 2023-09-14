;; core-benchmarking.el --- Initialize core-benchmarking -*- lexical-binding: t; -*-

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
;;  Source:
;;  https://github.com/purcell/emacs.d/blob/master/lisp/init-benchmarking.el
;;

;;; Code:

(defun elemacs-time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))


(defvar elemacs-require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun elemacs-require-times-wrapper (orig feature &rest args)
  "Note in `elemacs-require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        (apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (elemacs-time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'elemacs-require-times
                       (list feature require-start-time time)
                       t))))))

(advice-add 'require :around 'elemacs-require-times-wrapper)


(define-derived-mode elemacs-require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 elemacs-require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 elemacs-require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  ;; (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'elemacs-require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun elemacs-require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun elemacs-require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun elemacs-require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in elemacs-require-times
           with order = 0
           do (cl-incf order)
           collect (list order
                         (vector
                          (format "%.3f" (elemacs-time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun elemacs-require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (elemacs-require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))




(defun elemacs-show-init-time ()
  (message "init completed in %.2fms"
           ;; (elemacs-time-subtract-millis after-init-time before-init-time)
           (* 1000 (float-time (time-since before-init-time)))))

(add-hook 'window-setup-hook 'elemacs-show-init-time)

(provide 'core-benchmarking)
;;; core-benchmarking.el ends here.
