;;; souyun.el --- parse poem -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

;; Version: 0.1
;; Package-Requires: nil
;; Keywords: poem
;; SPDX-License-Identifier: GPL-3.0-or-later

(defgroup souyun nil
  "Provide Chinese poetry rhyme query.")

(defcustom souyun-chardict-path "~/src/Clone/pingShuiYun/data/baseCharDict.json"
  "A file of char dict."
  :group 'souyun
  :type 'directory)

(defvar souyun-chardich-hash nil)
(defvar souyun-overlays nil)

(defun souyun--parse-dict ()
  (with-temp-buffer
    (insert-file-contents souyun-chardict-path)
    (goto-char (point-min))
    (setq souyun-chardich-hash (json-parse-buffer))))

(defun souyun--parse-sentence (sentence)
  (unless souyun-chardich-hash
    (souyun--parse-dict))
  (mapcar (lambda (word)
            (let ((result (gethash word souyun-chardich-hash)))
              (cons word
                    (mapcar (lambda (vec)
                              (cons (aref vec 0)
                                    (aref vec 1)))
                            result))))
          (split-string sentence "" t)))

(defun souyun--get-four-tones (sentence)
  (mapconcat (lambda (entry)
               (let ((result (mapcar #'car (cdr entry))))
                 (cond
                  ((length= result 1)
                   (car result))
                  ((length> result 1)
                   (if-let* ((result (replace-regexp-in-string
                                      "\\(\\w+\\)\\1+" "\\1"
                                      (mapconcat #'identity result)))
                             ((length> result 1)))
                       (format "[%s]" result)
                     result))
                  (t (car entry)))))
             (souyun--parse-sentence sentence)))

(defun souyun--get-tone-patterns (sentence)
  (cl-flet ((get-tone-pattern (tone) (if (string= tone "平") "平" "仄")))
    (mapconcat (lambda (entry)
                 (let ((result (mapcar #'car (cdr entry))))
                   (cond
                    ((length= result 1)
                     (get-tone-pattern (car result)))
                    ((length> result 1)
                     (let ((patterns (mapconcat #'get-tone-pattern result)))
                       (if (string-match-p "^平+$\\|^仄+$" patterns)
                           (substring patterns 0 1)
                         ;; (format "[%s]" (replace-regexp-in-string
                         ;;                 "\\(\\w+\\)\\1+" "\\1"
                         ;;                 patterns))
                         "中"
                         )))
                    (t (car entry)))))
               (souyun--parse-sentence sentence))))

(defun souyun--get-rhymes (sentence)
  (when (string-match ".*?\\(.\\)\\(?:[。？！]\\|$\\)" sentence)
    (let  ((rhyme (match-string 1 sentence))
           (data (souyun--parse-sentence sentence)))
      (mapconcat
       #'cdr
       (alist-get rhyme data nil nil #'string=)
       " "))))

;;;###autoload
(defun souyun-clear ()
  (interactive)
  (dolist (ov souyun-overlays)
    (delete-overlay ov))
  (setq souyun-overlays nil))

(defmacro souyun-make-overlay (beg end func)
  (declare (indent defun))
  `(let* ((sentences (split-string (buffer-substring-no-properties ,beg ,end) "\n"))
          (tones (mapcar ,func sentences)))
     (save-excursion
       (goto-char ,beg)
       (while (and (< (point) ,end)
                   (re-search-forward "[。？！]" nil t))
         (end-of-line)
         (let ((ov (make-overlay (point) (1+ (point)))))
           (overlay-put ov 'after-string (propertize (concat
                                                      (unless (eq (char-after) 10)
                                                        "\n")
                                                      (car tones)
                                                      "\n")
                                                     'face 'shadow))
           (pop tones)
           (push ov souyun-overlays))))))

;;;###autoload
(defun souyun-get-four-tones (beg end)
  (interactive "r")
  (deactivate-mark)
  (souyun-make-overlay beg end #'souyun--get-four-tones))

;;;###autoload
(defun souyun-get-tone-patterns (beg end)
  (interactive "r")
  (deactivate-mark)
  (souyun-make-overlay beg end #'souyun--get-tone-patterns))

;;;###autoload
(defun souyun-get-rhymes (beg end)
  (interactive "r")
  (deactivate-mark)
  (let* ((sentences (split-string (buffer-substring-no-properties beg end) "\n"))
         (rhymes (mapcar #'souyun--get-rhymes sentences)))
    (save-excursion
      (goto-char beg)
      (while (and (< (point) end)
                  (re-search-forward "[。？！]" nil t))
        (end-of-line)
        (let ((ov (make-overlay (- (point) 2) (1- (point)))))
          (overlay-put ov 'after-string (propertize (format "(%s)"
                                                            (pop rhymes))
                                                    'face 'shadow))
          (push ov souyun-overlays))))))

;;;###autoload
(defun souyun-get-overview (beg end)
  (interactive "r")
  (souyun-clear)
  (souyun-get-rhymes beg end)
  (souyun-get-tone-patterns beg end))

;;;###autoload
(defun souyun-query (string)
  (interactive "sInput a word: ")
  (when (length> string 1)
    (user-error "Only accept one char!"))
  (if-let* ((result (gethash string souyun-chardich-hash))
            (rhymes (mapcar (lambda (vec) (cons (aref vec 0) (aref vec 1)))
                            result)))
      (message
       (mapconcat (lambda (rhyme) (concat (cdr rhyme) (car rhyme)))
                  rhymes " "))
    (message "No match!")))

(provide 'souyun)
;;; souyun.el ends here
