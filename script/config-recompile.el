(add-to-list 'load-path (concat user-emacs-directory "core"))
(add-to-list 'load-path (concat user-emacs-directory "lib"))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(eval-and-compile ; `borg'
  (defvar single-autoload-path (expand-file-name "etc/borg/autoload/" user-emacs-directory)
    "single autoload file.")

  (add-to-list 'load-path (expand-file-name "site-lisp/borg" user-emacs-directory))

  (defun lld-collect-autoloads (file)
    "insert all enabled drone's autoloads file to a single file."
    (make-directory (file-name-directory file) 'parents)

    ;; cleanup obsolete autoloads file
    (dolist (f (directory-files single-autoload-path t "autoload-[0-9]+-[0-9]+\\.elc?\\'"))
	  (unless (string= file f)
        (delete-file f)))

    (message "Generating single big autoload file.")
    (condition-case-unless-debug e
        (with-temp-file file
		  (setq-local coding-system-for-write 'utf-8)
		  (let ((standard-output (current-buffer))
                (print-quoted t)
                (print-level nil)
                (print-length nil)
                (home (expand-file-name "~"))
                path-list
                theme-path-list
                drones-path
                auto)
            (insert ";; -*- lexical-binding: t; coding: utf-8; no-native-compile: t -*-\n"
                    ";; This file is generated from enabled drones.\n")

            ;; replace absolute path to ~
            (dolist (p load-path)
			  ;; collect all drone's load-path
			  (when (string-prefix-p (expand-file-name user-emacs-directory) (expand-file-name p))
                (push p drones-path))

			  (if (string-prefix-p home p)
				  (push (concat "~" (string-remove-prefix home p)) path-list)
                (push p path-list)))

            (dolist (p custom-theme-load-path)
			  (if (and (stringp p)
					   (string-prefix-p home p))
				  (push (concat "~" (string-remove-prefix home p)) theme-path-list)
                (push p theme-path-list)))

            (prin1 `(set `load-path ',path-list))
            (insert "\n")
            (print `(set `custom-theme-load-path ',(nreverse theme-path-list)))
            (insert "\n")

            ;; insert all drone's autoloads.el to this file
            (dolist (p drones-path)
			  (when (file-exists-p p)
                (setq auto (car (directory-files p t ".*-autoloads.el\\'")))
                (when (and auto
						   (file-exists-p auto))
				  (insert-file-contents auto))))
            ;; remove all #$ load code
            (goto-char (point-min))
            (while (re-search-forward "\(add-to-list 'load-path.*#$.*\n" nil t)
			  (replace-match ""))

            ;; write local variables region
            (goto-char (point-max))
            (insert  "\n"
                     "\n;; Local Variables:"
                     "\n;; version-control: never"
                     "\n;; no-update-autoloads: t"
                     "\n;; End:"
                     ))
		  t)
	  (error (delete-file file)
             (signal 'collect-autoload-error (list file e)))))

  (defun lld-initialize ()
    (let ((file (concat single-autoload-path
                        "autoload-"
                        (format-time-string
                         "%+4Y%m%d-%H%M%S"
                         (file-attribute-modification-time
						  (file-attributes "~/.emacs.d/.gitmodules")))
                        ".el")))
	  (if (file-exists-p file)
		  (load file nil t)
        (require 'borg)
        (borg-initialize)
        (lld-collect-autoloads file))))

  (defun latest-file (path)
	"Get latest file (including directory) in PATH."
	(file-name-nondirectory (car (seq-find
								  (lambda (x) (not (nth 1 x))) ; non-directory
								  (sort
								   (directory-files-and-attributes path 'full nil t)
								   (lambda (x y) (time-less-p (nth 5 y) (nth 5 x))))))))

  (defun eli/collect-lib-autoloads ()
	(unless (string= (latest-file "~/.emacs.d/lib/") "lib-autoloads.el")
	  (require 'loaddefs-gen nil t)
	  (loaddefs-generate "~/.emacs.d/lib/"
						 "~/.emacs.d/lib/lib-autoloads.el"
						 nil nil nil t))
	(load "~/.emacs.d/lib/lib-autoloads.el" nil t))

  (lld-initialize)
  (eli/collect-lib-autoloads))

(require 'core-lib)
(require 'core-incremental-loading)
(require 'core-setup)
(require 'core-ui)
(require 'core-better-default)

(defun eli/collect-features (path)
  "Collect features from a config file."
  (with-temp-buffer
	(if (file-directory-p path)
		(dolist (file (directory-files path t ".el$"))
		  (insert-file-contents-literally file))
	  (insert-file-contents-literally path))
	(cl-loop with sexp
			 while (setq sexp (condition-case _
								  (read (current-buffer))
								(error nil)))
			 when (eq (car sexp) 'setup)
			 collect (cadr sexp) into features
			 finally (return features))))

(require 'borg)
(require 'epkg)
(require 'mood-line)
(require 'mindre-theme)

(require 'all-the-icons)

(message "---------Loading all features-----------")
(dolist (feat (eli/collect-features "~/.emacs.d/lisp/"))
  (unless (memq feat '(ess ess-smart-equals))
	(require feat nil t)))
(message "---------Loading all features done!-----------")

(message "---------byte+native core-----------")
(dolist (f (directory-files "~/.emacs.d/core/" t "el$"))
  (byte-compile-file f t)
  (native-compile f))
(message "---------byte+native core done-----------")

(message "---------byte+native lisp-----------")
(dolist (f (directory-files "~/.emacs.d/lisp/" t "el$"))
  (byte-compile-file f t)
  (native-compile f))
(message "---------byte+native lisp done-----------")

(message "---------byte+native lib-----------")
(dolist (f (directory-files "~/.emacs.d/lib/" t "^lib.*el$"))
  (byte-compile-file f t)
  (native-compile f))
(message "---------byte+native lib done-----------")

(native-compile "~/.emacs.d/init.el")
