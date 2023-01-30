;; init-vc.el --- Initialize vc configurations.	-*- lexical-binding: t -*-

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

(with-eval-after-load 'magit
  (setq imperative-verb-file "~/.emacs.d/private/imperatives.txt")
  (defun get-imperative-verbs ()
    "Return a list of imperative verbs."
    (let ((file-path imperative-verb-file))
      (with-temp-buffer
	    (insert-file-contents file-path)
	    (split-string (buffer-string) "\n" t))))

  ;; Parallels `git-commit-style-convention-checks',
  ;; allowing the user to specify which checks they
  ;; wish to enforce.
  (defcustom my-git-commit-style-convention-checks '(summary-starts-with-capital
                                                     summary-does-not-end-with-period
                                                     summary-uses-imperative)
    "List of checks performed by `my-git-commit-check-style-conventions'.
Valid members are `summary-starts-with-capital',
`summary-does-not-end-with-period', and
`summary-uses-imperative'. That function is a member of
`git-commit-finish-query-functions'."
    :options '(summary-starts-with-capital
               summary-does-not-end-with-period
               summary-uses-imperative)
    :type '(list :convert-widget custom-hood-convert-widget)
    :group 'git-commit)

  ;; Parallels `git-commit-check-style-conventions'
  (defun my-git-commit-check-style-conventions (force)
    "Check for violations of certain basic style conventions.

For each violation ask the user if she wants to proceed anway.
Option `my-git-commit-check-style-conventions' controls which
conventions are checked."
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (git-commit-summary-regexp) nil t)
      (let ((summary (match-string 1))
            (second-word))
	    (and
         (or (not (memq 'summary-does-not-end-with-period
                        my-git-commit-style-convention-checks))
             (not (string-match-p "[\\.!\\?;,:]$" summary))
             (y-or-n-p "Summary line ends with punctuation.  Commit anyway? "))
         (or (not (memq 'summary-uses-imperative
                        my-git-commit-style-convention-checks))
             (progn
               (string-match "^[[:alpha:]]*(?[^[:space:]()]*)?\\:[[:space:]]\\([[:alpha:]]*\\)" summary)
               (setq second-word (downcase (match-string 1 summary)))
               (car (member second-word (get-imperative-verbs))))
             (when (y-or-n-p "Summary line should use imperative.  Does it? ")
               (when (y-or-n-p (format "Add `%s' to list of imperative verbs?" second-word))
                 (with-temp-buffer
                   (insert second-word)
                   (insert "\n")
                   (write-region (point-min) (point-max) imperative-verb-file t)))
               t))))))


  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
	    git-commit-summary-max-length 50
	    git-commit-fill-column 72)
  (defun mu-magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  (keymap-set magit-status-mode-map "q" #'mu-magit-kill-buffers)
  (add-to-list 'git-commit-finish-query-functions
               #'my-git-commit-check-style-conventions))

;; (with-eval-after-load 'magit
;;   (magit-add-section-hook 'magit-status-sections-hook
;;                           'magit-insert-modules
;;                           'magit-insert-stashes
;;                           'append))

(with-eval-after-load 'magit
  (setq magit-todos-auto-group-items 3)
  (with-eval-after-load 'magit
    (magit-todos-mode)))

;; forge
(with-eval-after-load 'magit
  (require 'forge)
  (setq forge-owned-accounts '(("eli" . (remote-name "personal")))))

(with-eval-after-load 'magit
  ;; to use `org-protocol-git-clone', copy the following code then save it as a
  ;; bookmark:
  ;; javascript:location.href='org-protocol://git-clone?url=%27 + encodeURIComponent(window.getSelection());
  ;; (require 'org-protocol)
  (add-to-list 'org-protocol-protocol-alist
               '("git-clone"
                 :protocol "git-clone"
                 :function org-protocol-git-clone))

  (setq magit-clone-default-directory (expand-file-name (expand-file-name "src/Clone/" (getenv "HOME"))))
  (setq magit-clone-set-remote.pushDefault t)

  (defun org-protocol-git-clone (info)
    "Process an org-protocol://git-clone style url with INFO."
    (require 'magit-clone)
    (when-let ((url (plist-get info :url)))
      (magit-clone-regular url magit-clone-default-directory nil))
    nil)

  ;; from: https://github.com/ksqsf/emacs-config/blob/master/modules/prelude-git.el
  (defun github-parse-remote-url (remote)
    "Parse a git remote hosted on github to a (user,repo) pair.
This function returns nil if it cannot parse REMOTE."
    (let ((ssh-regexp "git@github\\.com:\\(.*\\)/\\(.*\\)\\.git")
          (https-regexp "https://github.com/\\(.*\\)/\\(.*\\)\\.git"))
      (let ((maybe-ssh (string-match ssh-regexp remote)))
        (if maybe-ssh
            (cons (match-string-no-properties 1 remote) (match-string-no-properties 2 remote))
          (let ((maybe-https (string-match https-regexp remote)))
            (if maybe-https
                (cons (match-string-no-properties 1 remote) (match-string-no-properties 2 remote))
              nil))))))

  (defun github-copy-reference-url-at-point ()
    "Copy a link to the current line on the GitHub Web interface."
    (interactive)
    (save-buffer)
    (let* ((remote (magit-get-remote))
           (remote-url (magit-git-str "remote" "get-url" remote))
           (commit (magit-rev-parse "--short" "HEAD"))
           (user/repo (github-parse-remote-url remote-url))
           (relative-path (file-relative-name buffer-file-name
                                              (vc-git-root buffer-file-name)))
           (locator (if (use-region-p)
                        (format "L%d,L%d"
                                (line-number-at-pos (use-region-beginning))
                                (line-number-at-pos (use-region-end)))
                      (format "L%d" (line-number-at-pos))))
           (url (if (member (prefix-numeric-value current-prefix-arg) '(4 16 64))
                    (format "https://github.com/%s/%s/blob/%s/%s#%s"
                            (car user/repo) (cdr user/repo) commit
                            relative-path locator)
                  (format "https://github.com/%s/%s"
                          (car user/repo) (cdr user/repo)))))
      (kill-new url)
      (browse-url url)
      (message "Open %s" url))))

(provide 'init-vc)
;;; init-vc.el ends here.
