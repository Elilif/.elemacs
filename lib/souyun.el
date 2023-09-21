;;; souyun.el --- parse poem -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

;; Version: 0.1
;; Package-Requires: nil
;; Keywords: poem
;; SPDX-License-Identifier: GPL-3.0-or-later

(defgroup souyun nil
  "Provide Chinese poetry rhyme query."
  :group 'applications)

(defcustom souyun-chardict-path "~/src/Clone/pingShuiYun/data/baseCharDict.json"
  "A file of char dict."
  :group 'souyun
  :type 'directory)

(defcustom souyun-pingshuiyundict-path "~/src/Clone/pingShuiYun/data/oriYunDict.json"
  "A file of char dict."
  :group 'souyun
  :type 'directory)

(defvar souyun-chardict-hash nil)
(defvar souyun-pingshuiyundict-hash nil)
(defvar souyun-overlays nil)

(defun souyun--parse-chardict ()
  (with-temp-buffer
    (insert-file-contents souyun-chardict-path)
    (goto-char (point-min))
    (setq souyun-chardict-hash (json-parse-buffer))))

(defun souyun--parse-pingshuiyundict ()
  (with-temp-buffer
    (insert-file-contents souyun-pingshuiyundict-path)
    (goto-char (point-min))
    (setq souyun-pingshuiyundict-hash (json-parse-buffer))))

(defun souyun--parse-sentence (sentence)
  (unless souyun-chardict-hash
    (souyun--parse-chardict))
  (mapcar (lambda (word)
            (let ((result (gethash word souyun-chardict-hash)))
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
  `(let* ((sentences (split-string (buffer-substring-no-properties ,beg ,end)
                                   "[。？！]"
                                   t "\n"))
          (tones (mapcar ,func sentences)))
     (save-excursion
       (goto-char ,beg)
       (while (and (< (point) ,end)
                   (re-search-forward "[。？！]" nil t))
         (let ((ov (make-overlay (point) (point))))
           (overlay-put ov 'after-string (propertize (concat
                                                      "\n"
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
  (let* ((sentences (split-string (buffer-substring-no-properties beg end)
                                  "[。？！]"
                                  t "\n"))
         (rhymes (mapcar #'souyun--get-rhymes sentences)))
    (save-excursion
      (goto-char beg)
      (while (and (< (point) end)
                  (re-search-forward "[。？！]" nil t))
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
(defun souyun-query-char (char)
  (interactive "sInput a character: ")
  (when (length> char 1)
    (user-error "Only accept one char!"))
  (unless souyun-chardict-hash
    (souyun--parse-chardict))
  (if-let* ((result (gethash char souyun-chardict-hash))
            (rhymes (mapcar (lambda (vec) (cons (aref vec 0) (aref vec 1)))
                            result)))
      (if current-prefix-arg
          (souyun--show-result
           (souyun--query-pingshuiyun
            (mapconcat (lambda (rhyme) (concat (cdr rhyme) (car rhyme)))
                       rhymes " ")))
        (message (mapconcat (lambda (rhyme) (concat (cdr rhyme) (car rhyme)))
                            rhymes " ")))
    (user-error "No match!")))

(defun souyun--query-pingshuiyun (rhymes)
  (unless souyun-pingshuiyundict-hash
    (souyun--parse-pingshuiyundict))
  (mapcar (lambda (rhyme)
            (cons rhyme (gethash rhyme souyun-pingshuiyundict-hash)))
          (split-string rhymes " ")))

(defun souyun--show-result (result)
  (let ((buf (get-buffer-create "*souyun*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          (erase-buffer)
          (mapc (lambda (rhyme)
                  (insert (car rhyme))
                  (insert "\n\n")
                  (insert (cdr rhyme))
                  (insert "\n\n"))
                result)
          (souyun-mode))))
    (pop-to-buffer buf)))

;;;###autoload
(defun souyun-query-pingshuiyun (rhyme)
  (interactive (list (completing-read "Select: "
                                      (hash-table-keys
                                       (or souyun-pingshuiyundict-hash
                                           (souyun--parse-pingshuiyundict))))))
  (souyun--show-result (souyun--query-pingshuiyun rhyme)))

(define-derived-mode souyun-mode special-mode "SouYun")

(keymap-set souyun-mode-map "q" #'quit-window)

(provide 'souyun)
;;; souyun.el ends here
