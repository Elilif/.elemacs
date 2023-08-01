;; lib-svg-tag-mode.el --- Initialize lib-svg-tag-mode configurations.	-*- lexical-binding: t; -*-

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

(defun svg-lib-tag (label &optional style &rest args)
  "Create an image displaying LABEL in a rounded box using given STYLE
and style elements ARGS."

  (let* ((default svg-lib-style-default)
         (style (if style (apply #'svg-lib-style nil style) default))
         (style (if args  (apply #'svg-lib-style style args) style))

         (foreground  (plist-get style :foreground))
         (background  (plist-get style :background))

         (crop-left   (plist-get style :crop-left))
         (crop-right  (plist-get style :crop-right))

         (alignment   (plist-get style :alignment))
         (stroke      (plist-get style :stroke))
         ;; (width       (plist-get style :width))
         (height      (plist-get style :height))
         (radius      (plist-get style :radius))
         ;; (scale       (plist-get style :scale))
         (margin      (plist-get style :margin))
         (padding     (plist-get style :padding))
         (font-size   (plist-get style :font-size))
         (font-family (plist-get style :font-family))
         (font-weight (plist-get style :font-weight))

         ;; use `fixed-pitch' while in `mixed-pitch-mode'
         (txt-char-width  (window-font-width nil 'fixed-pitch))
         (txt-char-height (window-font-height nil 'fixed-pitch))
         (txt-char-height (if line-spacing
                              (+ txt-char-height line-spacing)
                            txt-char-height))
         (font-info       (font-info (format "%s-%d" font-family font-size)))
         (font-size       (aref font-info 2)) ;; redefine font-size
         ;; (ascent          (aref font-info 8))
         (ascent          (plist-get style :ascent))
         (scale          (plist-get style :scale))
         (tag-char-width  (aref font-info 11))
         ;; (tag-char-height (aref font-info 3))
         (tag-width       (* (+ (length label) padding) txt-char-width))
         (tag-height      (* txt-char-height height))

         (svg-width       (+ tag-width (* margin txt-char-width)))
         (svg-height      tag-height)

         (tag-x  (* (- svg-width tag-width)  alignment))
         (text-x (+ tag-x (/ (- tag-width (* (length label) tag-char-width))
                             2)))
         (text-y ascent)

         (tag-x      (if crop-left  (- tag-x     txt-char-width) tag-x))
         (tag-width  (if crop-left  (+ tag-width txt-char-width) tag-width))
         (text-x     (if crop-left  (- text-x (/ stroke 2)) text-x))
         (tag-width  (if crop-right (+ tag-width txt-char-width) tag-width))
         (text-x     (if crop-right (+ text-x (/ stroke 2)) text-x))

         (svg (svg-create svg-width svg-height)))

    (if (>= stroke 0.25)
        (svg-rectangle svg tag-x 0 tag-width tag-height
                       :fill foreground :rx radius))
    (svg-rectangle svg (+ tag-x (/ stroke 2.0)) (/ stroke 2.0)
                   (- tag-width stroke) (- tag-height stroke)
                   :fill background :rx (- radius (/ stroke 2.0)))
    (svg-text svg label
              :font-family font-family :font-weight font-weight
              :font-size font-size :fill foreground :x text-x :y  text-y)
    (svg-lib--image svg :ascent 'center :scale scale)))

(defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{3\\}\\(?: [.+]?\\+[0-9]+[dwmy]\\)?")
(defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

(defun svg-progress-percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0) nil
                                    :margin 0 :stroke 2 :radius 3
                                    :padding 2 :width 4 :height 0.4
                                    :foreground "#B0BEC5")
              (svg-lib-tag (concat value "%") nil
                           :stroke 0 :margin 0 :foreground "#384d57"
                           :ascent 12))
             :ascent 70 :scale 0.7))

(defun svg-progress-count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ count total) nil
                                      :margin 0 :stroke 2 :radius 3
                                      :padding 2 :width 4 :height 0.4
                                      :foreground "#B0BEC5")
                (svg-lib-tag value nil
                             :stroke 0 :margin 0 :foreground "#384d57"
                             :ascent 12))
               :ascent 70 :scale 0.7)))

(defun eli/org-agenda-show-svg ()
  (let* ((case-fold-search nil)
         (keywords (mapcar #'svg-tag--build-keywords svg-tag--active-tags))
         (keyword (car keywords)))
    (while keyword
      (save-excursion
        (while (re-search-forward (nth 0 keyword) nil t)
          (put-text-property (match-beginning 0) (match-end 0)
							 'display  (nth 3 (eval (nth 2 keyword)))) ))
      (pop keywords)
      (setq keyword (car keywords)))))

(defun eli/svg-tag-todo-keywords-tag (tag face)
  (let ((scale 0.9))
    (when (org-at-heading-p)
      (let* ((level (number-to-string (org-current-level)))
             (face (intern (concat "org-level-" level)))
             (height (face-attribute face :height)))
        (setq scale (- height 0.1))))
    (svg-tag-make tag :face face
				  :inverse t :margin 0
				  :height 1.1 :ascent 16
                  :scale scale)))


;;;; provide
(provide 'lib-svg-tag-mode)
;;; lib-svg-tag-mode.el ends here.
