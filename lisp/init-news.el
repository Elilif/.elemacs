;; init-news.el --- Initialize news configurations.	-*- lexical-binding: t -*-

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

(setq shr-width 90)

(with-eval-after-load 'elfeed
  (add-hook 'elfeed-show-mode-hook #'visual-fill-column-mode)
  (add-hook 'elfeed-show-mode-hook #'hide-mode-line-mode)
  (add-hook 'elfeed-search-update-hook #'hide-mode-line-mode)
  (add-hook 'elfeed-search-update-hook #'hl-line-mode)
  (setq shr-inhibit-images nil)
  (setq elfeed-curl-extra-arguments '("-x" "http://127.0.0.1:7890"))
  (defun eli/elfeed-search-quit-and-kill-buffers ()
    "Save the database, then kill elfeed buffers, asking the user
for confirmation when needed."
    (interactive)
    (elfeed-db-save)
    (let (buf)
      (dolist (file rmh-elfeed-org-files)
	(setq buf (get-file-buffer file))
	(when (and (buffer-modified-p buf)
		   file
		   (y-or-n-p (format "Save file %s? " file)))
          (with-current-buffer buf (save-buffer)))
	(kill-buffer buf)))
    (kill-buffer "*elfeed-log*")
    (kill-buffer (current-buffer)))
  (keymap-set elfeed-search-mode-map "q"
	      #'eli/elfeed-search-quit-and-kill-buffers)
  (keymap-set elfeed-show-mode-map "q"
	      #'kill-buffer-and-window)
  (setq elfeed-show-entry-switch #'pop-to-buffer)
  (setq elfeed-search-filter "@2-days-ago +unread +A")
  
  (defun eli/filter-read ()
    (interactive)
    (elfeed-search-set-filter "@7-days-ago -unread +A"))
  
  (keymap-set elfeed-search-mode-map "R" #'eli/filter-read)

  ;; face for starred articles
  (defface elfeed-search-starred-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry."
    :group 'elfeed)
  (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'starred))
  (keymap-set elfeed-search-mode-map "m" #'elfeed-toggle-star)
  ;; Filter elfeed search buffer by the feed under cursor.
  (defun eli/elfeed-search-filter-source (entry)
    "Filter elfeed search buffer by the feed under cursor."
    (interactive (list (elfeed-search-selected :ignore-region)))
    (when (elfeed-entry-p entry)
      (elfeed-search-set-filter
       (concat
	"@6-months-ago "
	;; "+unread "
	"="
	(replace-regexp-in-string
	 (rx "?" (* not-newline) eos)
	 ""
	 (elfeed-feed-url (elfeed-entry-feed entry)))))))
  (keymap-set elfeed-search-mode-map "f" #'eli/elfeed-search-filter-source)

  (defun eli/elfeed-search-starred-entries ()
    (interactive)
    (elfeed-search-set-filter "+starred"))
  (keymap-set elfeed-search-mode-map "M" #'eli/elfeed-search-starred-entries))


(with-eval-after-load 'elfeed
  (setq elfeed-score-serde-score-file "~/.emacs.d/private/elfeed.score")
  (setq elfeed-score-rule-stats-file "~/.emacs.d/private/elfeed.stats")
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map))
;; (setq elfeed-search-print-entry-function #'elfeed-score-print-entry)

(with-eval-after-load 'elfeed
  (elfeed-org)
  (setq  rmh-elfeed-org-files (list "~/.emacs.d/private/elfeed.org")))

(add-to-list 'load-path "~/.emacs.d/site-lisp/elfeed-goodies/")
(with-eval-after-load 'elfeed
  (require 'elfeed-goodies)
  (setq elfeed-goodies/date-format "%Y-%m-%d")
  (elfeed-goodies/setup)
  (advice-add 'elfeed-goodies/show-mode-setup :after
	      (lambda ()
		(define-key elfeed-show-mode-map (kbd "M-v") nil))))

(with-eval-after-load 'mu4e
  (add-hook 'mu4e-view-mode-hook #'visual-fill-column-mode)
  (add-hook 'mu4e-view-mode-hook #'hide-mode-line-mode)
  (setq user-full-name "Eli Qian")
  (setq smtpmail-smtp-user "eli.q.qian@gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl
        smtpmail-local-domain "gmail.com"
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server         "smtp.gmail.com"
        user-mail-address  "eli.q.qian@gmail.com")
  (setq sendmail-program "/usr/bin/msmtp")
  (setq message-send-mail-function 'sendmail-send-it)
  (setq send-mail-function 'sendmail-send-it)
  ;;Debug
  (setq smtpmail-debug-info t)
  (setq smtpmail-debug-verb t)
  ;; (setq message-sendmail-f-is-evil t
  ;;  message-sendmail-extra-arguments '("--read-envelope-from")
  ;;   message-send-mail-function 'message-send-mail-with-sendmail)
  )


(with-eval-after-load 'mu4e
  (setq mail-user-agent 'mu4e-user-agent)
  (add-to-list 'mm-discouraged-alternatives "text/richtext")
  (setq shr-use-colors nil)
  (setq mu4e-mu-binary "~/.emacs.d/lib/mu4e/build/mu/mu")
  (setq mu4e-compose-format-flowed t)
  (setq mu4e-get-mail-command "proxychains mbsync -a"
	mu4e-update-interval 600)
  ;; configure the bookmarks.
  (setq mu4e-bookmarks
	    '( ("flag:unread AND NOT flag:trashed AND NOT list:emacs-orgmode.gnu.org AND NOT list:emacs-devel.gnu.org"  "Unread messages"                  ?u)
	       ("NOT flag:trashed AND NOT list:emacs-orgmode.gnu.org AND NOT list:emacs-devel.gnu.org"                  "All messages"                     ?a)
           ("date:today..now AND NOT list:emacs-orgmode.gnu.org AND NOT list:emacs-devel.gnu.org"                   "Today's messages"                 ?t)
           ("date:7d..now AND NOT list:emacs-orgmode.gnu.org AND NOT list:emacs-devel.gnu.org"                      "Last 7 days"                      ?w)
           ("date:1d..now AND NOT list:emacs-orgmode.gnu.org AND NOT list:emacs-devel.gnu.org"                      "Last 1 days"                      ?y)
           ("list:emacs-orgmode.gnu.org"                                                                            "Org mode"                         ?o)
           ("list:emacs-devel.gnu.org"                                                                              "Emacs Devel"                      ?e)
           ("flag:f"                                                                                                "starred"                          ?m)
           ("maildir:/sent"                                                                                         "sent"                             ?s)
           ("maildir:/drafts"                                                                                       "drafts"                           ?d)
           ("mime:image/*"                                                                                          "Messages with images"             ?p)
	       ("maildir:/trash"                                                                                        "Trash"                            ?g)))

  ;; filter
  (defun eli/mu4e-search-filter-source ()
    (interactive)
    (let* ((msg (mu4e-message-at-point))
	   (sender-email (plist-get (car (plist-get msg :from)) :email)))
      (mu4e--search-execute (concat "from:" sender-email) nil)))

  ;; citation format
  (setq message-citation-line-format "On %a, %b %d %Y, %f wrote:\n")
  (setq message-citation-line-function #'message-insert-formatted-citation-line)

  (keymap-set mu4e-headers-mode-map "f" #'eli/mu4e-search-filter-source)
  (keymap-set mu4e-headers-mode-map "!" #'mu4e-headers-mark-for-refile)
  (keymap-set mu4e-headers-mode-map "r" #'mu4e-headers-mark-for-read)

  ;; override original `mu4e~view-activate-urls'
  (defun mu4e~view-activate-urls ()
    "Turn things that look like URLs into clickable things.
Also number them so they can be opened using `mu4e-view-go-to-url'."
    (let ((num 0))
      (save-excursion
        (setq mu4e~view-link-map ;; buffer local
	          (make-hash-table :size 32 :weakness nil))
        (goto-char (point-min))
        (while (re-search-forward mu4e~view-beginning-of-url-regexp nil t)
	      (let ((bounds (thing-at-point-bounds-of-url-at-point)))
	        (when bounds
	          (let* ((url (thing-at-point-url-at-point))
		             (ov (make-overlay (car bounds) (cdr bounds))))
	            (puthash (cl-incf num) url mu4e~view-link-map)
	            (add-text-properties
	             (car bounds)
	             (cdr bounds)
	             `(face mu4e-link-face
		                mouse-face highlight
		                mu4e-url ,url
		                keymap ,mu4e-view-active-urls-keymap
		                help-echo
		                "[mouse-1] or [M-RET] to open the link"))
	            (overlay-put ov 'invisible t)
	            (overlay-put ov 'after-string
			                 (propertize (format "\u200B[%d]" num)
				                         'face 'mu4e-url-number-face)))))))))
  (setq mu4e-header-info
        '((:bcc :name "Bcc" :shortname "Bcc" :help "Blind Carbon-Copy recipients for the message" :sortable t)
          (:cc :name "Cc" :shortname "Cc" :help "Carbon-Copy recipients for the message" :sortable t)
          (:changed :name "Changed" :shortname "Chg" :help "Date/time when the message was changed most recently" :sortable t)
          (:date :name "Date" :shortname " Date" :help "Date/time when the message was sent" :sortable t)
          (:human-date :name "Date" :shortname " Date" :help "Date/time when the message was sent" :sortable :date)
          (:flags :name "Flags" :shortname " Flgs" :help "Flags for the message" :sortable nil)
          (:from :name "From" :shortname " From" :help "The sender of the message" :sortable t)
          (:from-or-to :name "From/To" :shortname "From/To" :help "Sender of the message if it's not me; otherwise the recipient" :sortable nil)
          (:maildir :name "Maildir" :shortname "Maildir" :help "Maildir for this message" :sortable t)
          (:list :name "List-Id" :shortname " List" :help "Mailing list id for this message" :sortable t)
          (:mailing-list :name "List" :shortname " List" :help "Mailing list friendly name for this message" :sortable :list)
          (:message-id :name "Message-Id" :shortname "MsgID" :help "Message-Id for this message" :sortable nil)
          (:path :name "Path" :shortname "Path" :help "Full filesystem path to the message" :sortable t)
          (:size :name "Size" :shortname "Size" :help "Size of the message" :sortable t)
          (:subject :name "Subject" :shortname " Subject" :help "Subject of the message" :sortable t)
          (:tags :name "Tags" :shortname "Tags" :help "Tags for the message" :sortable nil)
          (:thread-subject :name "Subject" :shortname "Subject" :help "Subject of the thread" :sortable :subject)
          (:to :name "To" :shortname "To" :help "Recipient of the message" :sortable t)))
  )


(add-hook 'mu4e-main-mode-hook #'mu4e-alert-enable-notifications)
(with-eval-after-load 'mu4e
  (mu4e-alert-set-default-style 'notifications)
  (setq mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed AND NOT list:emacs-orgmode.gnu.org AND NOT list:emacs-devel.gnu.org"))


(with-eval-after-load 'visual-fill-column
  (setq-default visual-fill-column-center-text t)
  (setq-default visual-fill-column-width 100)
  (setq-default visual-fill-column-extra-text-width nil))

(provide 'init-news)
;;; init-news.el ends here.
