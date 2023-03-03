;; init-news.el --- Initialize news configurations.	-*- lexical-binding: t -*-

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

;;; TODO: remove compile warnings
(cl-eval-when (compile)
  (require 'elfeed)
  (require 'elfeed-score)
  (require 'mu4e)
  (require 'mu4e-view)
  (require 'lib-elfeed))

(defvar mu4e~view-link-map)
(defvar mu4e~view-beginning-of-url-regexp)

;;;; visual fill column
(setup visual-fill-column
  (:option*
   visual-fill-column-center-text t
   visual-fill-column-width 100
   visual-fill-column-extra-text-width nil))
;;;; elfeed
(setup elfeed
  (:iload* elfeed)
  (:when-loaded
	(require 'lib-elfeed))
  (:hooks elfeed-show-mode-hook visual-fill-column-mode
		  elfeed-show-mode-hook hide-mode-line-mode
		  elfeed-search-update-hook hide-mode-line-mode
		  elfeed-search-update-hook hl-line-mode)
  (:option*
   shr-width 90
   shr-inhibit-images t
   shr-use-colors nil

   elfeed-curl-extra-arguments '("-x" "http://127.0.0.1:7890")
   elfeed-search-filter "@2-days-ago +unread +A"
   elfeed-search-face-alist '((starred elfeed-search-starred-title-face)
							  (unread elfeed-search-unread-title-face)))
  (:bind-into elfeed-search-mode-map
	"q" eli/elfeed-search-quit-and-kill-buffers
	"R" eli/filter-read
	"m" elfeed-toggle-star
	"f" eli/elfeed-search-filter-source
	"M" eli/elfeed-search-starred-entries)
  (:bind-into elfeed-show-mode-map
	"q" kill-buffer-and-window))


;;;; elfeed org
(setup elfeed
  (:after elfeed
	(elfeed-org))
  (:option*
   rmh-elfeed-org-files '("~/.emacs.d/private/elfeed.org")))

;;;; elfeed score
(setup elfeed-score
  (:option*
   elfeed-score-serde-score-file "~/.emacs.d/private/elfeed.score"
   elfeed-score-rule-stats-file "~/.emacs.d/private/elfeed.stats")
  (:after elfeed
	(elfeed-score-enable)
	(define-key elfeed-search-mode-map "=" elfeed-score-map)))

;;;; elfeed googdies
(setup elfeed-googdies
  (:iload* elfeed)
  (:after elfeed
	(elfeed-goodies/setup)
	(:option*
	 elfeed-show-entry-switch #'pop-to-buffer)))
;;;; Mu4e
(setup mu4e
  (:iload* mu4e)
  (:hooks mu4e-view-mode-hook visual-fill-column-mode
		  mu4e-view-mode-hook hide-mode-line-mode)
  (:option*
   user-full-name "Eli Qian"
   smtpmail-smtp-user "eli.q.qian@gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 465
   smtpmail-stream-type 'ssl
   smtpmail-local-domain "gmail.com"
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server         "smtp.gmail.com"
   user-mail-address  "eli.q.qian@gmail.com"
   sendmail-program "/usr/bin/msmtp"
   message-send-mail-function 'sendmail-send-it
   send-mail-function 'sendmail-send-it
   smtpmail-debug-info t
   smtpmail-debug-verb t
   mail-user-agent 'mu4e-user-agent
   mm-discouraged-alternatives '("text/richtext")
   mu4e-mu-binary "~/src/Cradle/elemacs/site-lisp/mu4e/build/mu/mu"
   mu4e-compose-format-flowed t
   mu4e-get-mail-command "proxychains mbsync -a"
   mu4e-update-interval 600
   mu4e-bookmarks '(("flag:unread AND NOT flag:trashed AND NOT list:emacs-orgmode.gnu.org AND NOT list:emacs-devel.gnu.org"  "Unread messages"                  ?u)
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
					("maildir:/trash"                                                                                        "Trash"                            ?g))

   message-citation-line-format "On %a, %b %d %Y, %f wrote:\n"
   message-citation-line-function #'message-insert-formatted-citation-line
   )
  (:bind-into mu4e-headers-mode-map
	"f" eli/mu4e-search-filter-source
	"!" mu4e-headers-mark-for-refile
	"r" mu4e-headers-mark-for-read)
  (:when-loaded
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
			(:to :name "To" :shortname "To" :help "Recipient of the message" :sortable t)))))

;; filter
(defun eli/mu4e-search-filter-source ()
  (interactive)
  (let* ((msg (mu4e-message-at-point))
	     (sender-email (plist-get (car (plist-get msg :from)) :email)))
    (mu4e--search-execute (concat "from:" sender-email) nil)))

;;;; mu4e alert
(setup mu4e-alert
  (:hooks mu4e-main-mode-hook mu4e-alert-enable-notifications)
  (:after mu4e
	(mu4e-alert-set-default-style 'notifications))
  (:option*
   mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed AND NOT list:emacs-orgmode.gnu.org AND NOT list:emacs-devel.gnu.org"))


;;;; provide
(provide 'init-news)
;;; init-news.e ends here.
