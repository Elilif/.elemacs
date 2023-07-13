;; core-setup.el --- Initialize core-setup.	-*- lexical-binding: t; -*-

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
;; setup.el configs.
;; Add some local macros.
;;

;;; Code:

(cl-eval-when (compile)
  (require 'borg)
  (require 'info)
  (require 'epkg))

(require 'setup)
(require 'once-setup)



;;;; setup
;;;  custom local variables
(setup-define :iload
  (lambda (&rest packages)
    `(elemacs-load-packages-incrementally '(,@packages)))
  :documentation "Load packages incrementally.")


;; (defmacro epkg-get-subpkgs (pkg)
;;   "Get sub-package list from epkg."
;;   (let* ((name (symbol-name pkg))
;; 		 (provided (thread-last
;; 					 (oref (epkg name) provided)
;; 					 (mapcar (lambda (x) (car x)))))
;; 		 (no-byte-compile (thread-last
;; 							(borg-get-all name "no-byte-compile")
;; 							(mapcar (lambda (x) (file-name-base x))))))
;; 	`',(cl-remove-if (lambda (x) (let ((name (symbol-name x)))
;; 								   (or (string-match-p "test" name)
;; 									   (member name no-byte-compile))))
;; 					 provided)))

;; (setup-define :iload*
;;   (lambda (body)
;; 	`(once (list :packages 'epkg)
;; 	   (elemacs-load-packages-incrementally
;; 		(epkg-get-subpkgs ,body))))
;;   :documentation "Load packages incrementally.")


(setup-define :silence
  (lambda (&rest body)
    `(cl-letf (((symbol-function 'message) (lambda (&rest _args) nil)))
       ,(macroexp-progn body)))
  :documentation "Evaluate BODY but keep the echo era clean."
  :debug '(setup))

(setup-define :option*
  (setup-make-setter
   (lambda (name)
     `(funcall (or (get ',name 'custom-get)
                   #'symbol-value)
               ',name))
   (lambda (name val)
     `(progn
        (custom-load-symbol ',name)
        (funcall (or (get ',name 'custom-set) #'set-default)
                 ',name ,val))))

  :documentation "Like default `:option', but set variables after the feature is
loaded."
  :debug '(sexp form)
  :repeatable t
  :after-loaded t)

(setup-define :after
  (lambda (feature &rest body)
    `(:with-feature ,feature
       (:when-loaded ,@body)))
  :documentation "Eval BODY after FEATURE."
  :indent 1)

(setup-define :delay
  (lambda (time &rest body)
    `(run-with-idle-timer ,time nil
                          (lambda () ,@body)))
  :documentation "Delay loading BODY until a certain amount of idle time has passed."
  :indent 1)

;;  src: https://emacs.nasy.moe/#Setup-EL
(setup-define :autoload
  (lambda (func)
    (let ((fn (if (memq (car-safe func) '(quote function))
                  (cadr func)
                func)))
      `(unless (fboundp (quote ,fn))
         (autoload (function ,fn) ,(symbol-name (setup-get 'feature)) nil t))))
  :documentation "Autoload COMMAND if not already bound."
  :repeatable t
  :signature '(FUNC ...))

(setup-define :hooks
  (lambda (hook func)
    `(add-hook ',hook #',func))
  :documentation "Add pairs of hooks."
  :repeatable t)

(setup-define :init
  (lambda (&rest body) (macroexp-progn body))
  :documentation "Init keywords like use-package and leaf.")

(setup-define :advice
  (lambda (symbol where function)
    `(advice-add ',symbol ,where ,function))
  :documentation "Add a piece of advice on a function.
 See `advice-add' for more details."
  :after-loaded t
  :debug '(sexp sexp function-form)
  :ensure '(nil nil func)
  :repeatable t)


;;;; epkg
(setup epkg
  (:iload epkg)
  (:option* epkg-repository "~/.emacs.d/var/epkgs/"))

;;;; borg
(defun elemacs/borg-clean (clone)
  (let* ((path (borg--expand-load-path clone nil))
         (file (expand-file-name (format "%s-autoloads.el" clone) (car path))))
	(delete-file file)))

(setup borg
  (:iload borg)
  (:once (list :before 'info)
    (info-initialize)
    (borg-do-drones (drone)
      (dolist (dir (borg-info-path drone))
        (push  dir Info-directory-list))))
  (:option*
   borg-compile-function #'borg-byte+native-compile-async)
  (:when-loaded
    (advice-add 'borg-clean :after #'elemacs/borg-clean)))

;;;; provide
(provide 'core-setup)
;;; core-setup.el ends here.
