;;; radiance.el --- description -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

;; Version: 0.1
;; Package-Requires: requires
;; Keywords: keywords
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'rect)
(require 'cl-lib)

(defgroup radiance nil
  "Keyboard macro for marked objects."
  :group 'radiance)

(defface radiance-mark-face
  '((t (:background "pink")))
  "Face for marked regions."
  :group 'radiance)

(defface radiance-region-face
  '((t (:background "lightblue")))
  "Face for marked regions."
  :group 'radiance)

(defvar-local radiance-overlays nil)
(defvar-local radiance-regions nil)
(defvar-local radiance-current-overlay nil)

(defun radiance--get-current-mark-ov ()
  (cl-find-if
   (lambda (ov)
     (eq (overlay-get ov 'face)
         'radiance-mark-face))
   (overlays-in (1- (point)) (1+ (point)))))

(defun radiance--set-current-ov ()
  "Move the point to the beginning of the overlay."
  (let ((ov-at-point (radiance--get-current-mark-ov))
        (last-ov (car (last radiance-overlays))))
    (setq radiance-current-overlay (or ov-at-point last-ov))
    (goto-char (overlay-start radiance-current-overlay))))

(defun radiance--collect-in-region (beg end reg)
  "Highlight all matching parts for REG between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (and (not (eobp))
                (re-search-forward reg end t))
      (if-let* ((mbeg (match-beginning 0))
                (mend (match-end 0))
                (not-empty? (not (eq mbeg mend)))
                (ov (make-overlay mbeg mend nil nil t)))
          (progn
            (overlay-put ov 'face 'radiance-mark-face)
            (overlay-put ov 'priority 1)
            (add-to-list 'radiance-overlays ov t))
        (when (< (point) end)
          (forward-char 1))))))

(defun radiance-collect (reg)
  "Highlight all matching parts for REG."
  (if (not reg)
      (error "There is nothing.")
    (cond
     ((and radiance-regions
           (cl-find-if #'overlay-buffer radiance-regions))
      (dolist (region radiance-regions)
        (let ((beg (overlay-start region))
              (end (overlay-end region)))
          (save-excursion
            (save-restriction
              (narrow-to-region beg end)
              (radiance--collect-in-region beg end reg))))))
     (t
      (radiance--collect-in-region (point-min) (point-max) reg)))))

(defmacro radiance--perform (&rest body)
  (declare (indent defun))
  `(progn
     (when (region-active-p) (deactivate-mark))
     (when radiance-overlays
       (end-kbd-macro)
       (radiance-exit nil))
     ,@body
     (radiance--set-current-ov)
     (radiance-start)))

;;;###autoload
(defun radiance-mark-strings (string)
  "Mark all the same strings.

When the region is active, use the text in the region. Otherwise,
use the word at point."
  (interactive (list (cond
                      ((use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning)
                        (region-end)))
                      (t (thing-at-point 'word)))))
  (radiance--perform
    (radiance-collect string)))

;;;###autoload
(defun radiance-mark-symbols (symbol)
  "Mark all symbols the same as the one under the current cursor.

If there are marked regions, mark all symbols in these regions
only."
  (interactive (list (thing-at-point 'symbol)))
  (radiance--perform
    (radiance-collect (format "\\_<%s\\_>" symbol))))

;;;###autoload
(defun radiance-mark-lines ()
  "Mark all lines.

If there are marked regions, mark all lines in these regions
only."
  (interactive)
  (radiance--perform
    (radiance-collect "^.*$")))

;;;###autoload
(defun radiance-mark-region (beg end)
  "Mark the region between BEG and END.

This command is applicable to both normal regions and
`rectangle-mark-mode'. You can also mark multiple region at once."
  (interactive "r")
  (if (cl-find-if (lambda (ov)
                    (eq (overlay-get ov 'face)
                        'radiance-region-face))
                  (overlays-in (1- beg) (1+ end)))
      (error "Regions overlapped!")
    (let ((ov-adder (lambda (beg end)
                      (let ((ov (make-overlay beg end nil nil t)))
                        (overlay-put ov 'face 'radiance-region-face)
                        (add-to-list 'radiance-regions ov t)))))
      (cond
       ((bound-and-true-p rectangle-mark-mode)
        (let ((bounds (extract-rectangle-bounds beg end)))
          (dolist (bound bounds)
            (funcall ov-adder (car bound) (cdr bound)))))
       (t
        (funcall ov-adder beg end)))
      (deactivate-mark))))

;;;###autoload
(defun radiance-exit (arg)
  "Quit out of recording the macro or delete overlays.

With ARG, delete `radiance-overlays' and `radiance-regions',
delete `radiance-overlays' only otherwise."
  (interactive "P")
  (radiance-delete-overlays arg)
  (radiance-mode -1)

  ;; taken from `keyboard-quit'.
  (setq saved-region-selection nil)
  (let (select-active-regions)
    (deactivate-mark))
  (if (fboundp 'kmacro-keyboard-quit)
      (kmacro-keyboard-quit))
  (if defining-kbd-macro
      (force-mode-line-update t))
  (setq defining-kbd-macro nil))

(defun radiance-delete-overlays (arg)
  "With ARG, delete `radiance-overlays' and `radiance-regions',
delete `radiance-overlays' only otherwise."
  (when radiance-overlays
    (dolist (overlay radiance-overlays)
      (delete-overlay overlay))
    (setq radiance-overlays nil))
  (when (and arg radiance-regions)
    (dolist (overlay radiance-regions)
      (delete-overlay overlay))
    (setq radiance-regions nil)))

(defun radiance-start ()
  "Record subsequent keyboard input, defining a keyboard macro."
  (if (not radiance-overlays)
      (message "Nothing is selected.")
    (radiance-mode 1)
    (kmacro-start-macro 0)))

;;;###autoload
(defun radiance-finish (arg)
  "Finish defining a keyboard macro and apply the macro to all overlays."
  (interactive "P")
  (end-kbd-macro)
  (save-excursion
    (dolist (ov radiance-overlays)
      (unless (eq ov radiance-current-overlay)
        (goto-char (overlay-start ov))
        (call-last-kbd-macro))))
  (radiance-exit arg))

;;;###autoload
(defun radiance-end-of-line ()
  "Similar to `end-of-line' but respect the radiance region."
  (interactive)
  (let ((ov (radiance--get-current-mark-ov)))
    (goto-char (overlay-end ov))))

;;;###autoload
(defun radiance-beginning-of-line ()
  "Similar to `beginning-of-line' but respect the radiance retion."
  (interactive)
  (let ((ov (radiance--get-current-mark-ov)))
    (goto-char (overlay-start ov))))

(define-minor-mode radiance-mode
  "Minor mode for radiance."
  :keymap
  (let ((map (make-sparse-keymap)))
    (keymap-set map "s-`" #'radiance-finish)
    (keymap-set map "C-g" #'radiance-exit)
    (keymap-set map "C-a" #'radiance-beginning-of-line)
    (keymap-set map "C-e" #'radiance-end-of-line)
    map))

(provide 'radiance)
;;; radiance.el ends here
