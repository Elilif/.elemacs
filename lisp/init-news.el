;; init-news.el --- Initialize news configurations.     -*- lexical-binding: t -*-

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
  (:iload elfeed)
  (:when-loaded
    (require 'lib-elfeed))
  (:hooks elfeed-show-mode-hook visual-fill-column-mode
          ;; elfeed-show-mode-hook hide-mode-line-mode
          elfeed-search-update-hook hide-mode-line-mode
          elfeed-search-update-hook hl-line-mode
          midnight-hook elfeed-update)
  (:option*
   shr-width 90
   shr-inhibit-images nil
   shr-use-colors t

   elfeed-curl-extra-arguments '("-x" "http://127.0.0.1:7890")
   elfeed-search-filter "@2-days-ago +unread +A"
   elfeed-search-face-alist '((starred elfeed-search-starred-title-face)
                              (unread elfeed-search-unread-title-face)))
  (:bind-into elfeed-search-mode-map
    "q" eli/elfeed-search-quit-and-kill-buffers
    "R" eli/filter-read
    "o" eli/elfeed-overview
    "m" elfeed-toggle-star
    "f" eli/elfeed-search-filter-source
    "M" eli/elfeed-search-starred-entries
    "S" eli/elfeed-search-set-filter)
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
(setup elfeed-goodies
  (:iload elfeed-goodies)
  (:after elfeed
    (elfeed-goodies/setup)
    (setq elfeed-show-entry-switch #'pop-to-buffer
          ;; workround for `(eval (elfeed-goodies/entry-header-line))' error
          ;; elfeed-search-header-function #'elfeed-search--header
          )
    ;; (remove-hook 'elfeed-show-mode-hook #'elfeed-goodies/show-mode-setup)
    )
  (:advice elfeed-goodies/show-mode-setup :after
           (lambda () (keymap-set elfeed-show-mode-map "M-v" #'scroll-down-command))))
;;;; Mu4e
(setup mu4e
  (:iload mu4e)
  (:also-load lib-mu4e)
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
   mu4e-mu-binary "~/.emacs.d/site-lisp/mu4e/build/mu/mu"
   mu4e-compose-format-flowed t
   mu4e-get-mail-command "proxychains mbsync -a"
   mu4e-update-interval 1800
   mu4e-modeline-support nil
   mu4e-query-rewrite-function 'mu4e-goodies~break-cjk-query
   mu4e-bookmarks '(("flag:unread"                       "Unread messages"                  ?u)
                    ("NOT flag:trashed"                  "All messages"                     ?a)
                    ("date:today..now"                   "Today's messages"                 ?t)
                    ("date:7d..now"                      "Last 7 days"                      ?w)
                    ("date:1d..now"                      "Last 1 days"                      ?y)
                    ("list:emacs-orgmode.gnu.org"        "Org mode"                         ?o)
                    ("list:emacs-devel.gnu.org"          "Emacs Devel"                      ?e)
                    ("flag:f"                            "starred"                          ?m)
                    ("maildir:/sent"                     "sent"                             ?s)
                    ("maildir:/drafts"                   "drafts"                           ?d)
                    ("mime:image/*"                      "Messages with images"             ?p)
                    ("maildir:/trash"                    "Trash"                            ?g))

   message-citation-line-format "On %a, %b %d %Y, %f wrote:\n"
   message-citation-line-function #'message-insert-formatted-citation-line
   )
  (:bind-into mu4e-headers-mode-map
    "f" eli/mu4e-search-filter-source
    "!" mu4e-headers-mark-for-refile
    "r" mu4e-headers-mark-for-read)
  (:bind-into mu4e-main-mode-map
    "g" mu4e-update-index)
  (:when-loaded
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
