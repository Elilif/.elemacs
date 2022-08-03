;; init-spell.el --- Initialize spell configurations.	-*- lexical-binding: t -*-

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

(add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'emacs-lisp-mode-hook #'flyspell-prog-mode)
(with-eval-after-load 'flyspell
  (setq flyspell-mark-duplications-flag t
        flyspell-issue-message-flag nil))

;; ispell-set-spellchecker-params has to be called
;; before ispell-hunspell-add-multi-dic will work
(with-eval-after-load 'ispell
  (setq ispell-program-name "/usr/bin/hunspell")
  (setq ispell-dictionary "en_US-large,fr_FR")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US-large,fr_FR")
  (setq ispell-personal-dictionary "~/.emacs.d/mydictionaryfr-en")
  (add-to-list 'ispell-extra-args "--sug-mode=ultra"))

(elemacs-require-package 'flyspell-correct)
(with-eval-after-load 'flyspell
  (keymap-set flyspell-mode-map "C-;" #'flyspell-correct-wrapper)
  (keymap-set flyspell-mode-map "C-." nil))

(elemacs-require-package 'youdao-dictionary)
(keymap-global-set "C-c t" #'youdao-dictionary-search-at-point-posframe)
(with-eval-after-load 'youdao-dictionary
  (setq url-automatic-caching t)
  (defun youdao-dictionary-delete-newlines (&optional beg end)
    "Save the current region (or line) to the `kill-ring' after stripping extra whitespace and new lines"
    (interactive
     (if (region-active-p)
	 (list (region-beginning) (region-end))
       (list (line-beginning-position) (line-end-position))))
    (let ((my-text (buffer-substring-no-properties beg end)))
      (with-temp-buffer
	(insert my-text)
	(goto-char 1)
	(while (looking-at "[ \t\n]")
          (delete-char 1))
	(let ((fill-column 9333999))
          (fill-region (point-min) (point-max)))
	(buffer-substring-no-properties (point-min) (point-max)))))
  (defun youdao-dictionary--region-or-word ()
    "Return word in region or word at point."
    (if (derived-mode-p 'pdf-view-mode)
	(if (pdf-view-active-region-p)
            (mapconcat 'identity (pdf-view-active-region-text) "\n"))
      (if (use-region-p)
          (youdao-dictionary-delete-newlines (region-beginning)
                                             (region-end))
	(thing-at-point (if youdao-dictionary-use-chinese-word-segmentation
                            'chinese-or-other-word
                          'word)
			t)))))

(elemacs-require-package 'wordnut)
(keymap-global-set "C-c y" #'wordnut-lookup-current-word)

(defun Eli/dict-search ()
  (interactive)
  (let ((BASEDIR "~/Documents/txtdict/")
	(INIT-INPUT))
    (consult-ripgrep BASEDIR INIT-INPUT)))

(defun Eli/te-search ()
  (interactive)
  (let ((BASEDIR "~/Documents/TEdict")
	(INIT-INPUT "\\("))
    (consult-ripgrep BASEDIR INIT-INPUT)))

(provide 'init-spell)
;;; init-spell.el ends here.
