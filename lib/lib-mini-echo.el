;;; lib-mini-echo.el --- mini-echo config -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

(defvar eli/org-agenda-todo-statistics-icons '("" "󰮱" ""))
(defvar eli/mini-echo-org-todo-statistics "")

(defun eli/mini-echo-use-short-style-p ()
  (< (mini-echo-minibuffer-width) 100))

;;;###autoload
(defun eli/org-agenda-todo-statistics ()
  "Return the number of TODOs in each agenda block."
  (when (boundp 'org-agenda-buffer-name)
    (when-let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
      (with-current-buffer agenda-buffer
        (save-excursion
          (goto-char (point-min))
          (let ((lines '())
                (result '()))
            (dolist (key '("^Inbox" "^Next" "^Started" "^Projects"))
              (push (line-number-at-pos (re-search-forward key nil t)) lines))
            (dotimes (i 3)
              (push (concat
                     (nth i eli/org-agenda-todo-statistics-icons)
                     " "
                     (number-to-string (- (nth i lines)
                                          (nth (1+ i) lines)
                                          3))
                     " ")
                    result))
            (setq eli/mini-echo-org-todo-statistics (mapconcat #'identity  result))))))))

(mini-echo-define-segment "org-todo"
  "Return the org todo information"
  :fetch
  (when eli/mini-echo-org-todo-statistics
    (mini-echo-segment--print eli/mini-echo-org-todo-statistics 'mindre-faded))
  :update-advice '((org-agenda . :after)
                   (org-agenda-redo . :after)
                   (org-agenda-redo-all . :after))
  :update
  (progn
    (eli/org-agenda-todo-statistics)
    (mini-echo-update-overlays)))

(mini-echo-define-segment "org-clock"
  "Return the org clock information"
  :fetch
  (when (memq 'org-mode-line-string global-mode-string)
    (mini-echo-segment--print org-mode-line-string 'mindre-faded))
  :update-advice '((org-clock-update-mode-line . :after))
  :update-hook '(org-clock-out-hook)
  :update (when (memq 'org-mode-line-string global-mode-string)
            (mini-echo-update-overlays)))

(provide 'lib-mini-echo)
;;; lib-mini-echo.el ends here
