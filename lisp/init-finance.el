;; init-finance.el --- Initialize finance configurations.	-*- lexical-binding: t -*-

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
;; Calendar configuration.
;;

;;; Code:

(with-eval-after-load 'ledger-mode
  (setq ledger-reconcile-default-commodity "¥"
	ledger-post-amount-alignment-column 80
	ledger-report-auto-refresh-sticky-cursor t
	ledger-report-auto-refresh t
	ledger-copy-transaction-insert-blank-line-after t)
  (setq-default ledger-occur-use-face-shown nil)
  (setq ledger-reports '(("bal" "%(binary) -f %(ledger-file) bal")
                         ("bal this month" "%(binary) -f %(ledger-file) bal -p %(month) -S amount")
                         ("bal this year" "%(binary) -f %(ledger-file) bal -p 'this year'")
                         ("net worth"      "%(binary) -f %(ledger-file) bal Assets Liabilities")
                         ("reg" "%(binary) -f %(ledger-file) reg")
                         ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
                         ("account" "%(binary) -f %(ledger-file) reg %(account)"))))



(provide 'init-finance)
;;; init-finance.el ends here.
