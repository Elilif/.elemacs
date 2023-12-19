;;; pix2tex.el --- use pix2tex to  convert images of equations into LaTeX code -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

;; Version: 0.1
;; Package-Requires: (emacs "28.2")
;; Keywords: LaTex
;; SPDX-License-Identifier: GPL-3.0-or-later

(defgroup pix2tex nil
  "Convert screenshots to LaTeX equations."
  :group 'applications)


;; taken from https://github.com/jethrokuan/mathpix.el
(defcustom pix2tex-screenshot-method "flameshot gui --raw > %s"
  "The tool to capture screenshots."
  :type '(choice
          (const :tag "gnome-screenshot" "gnome-screenshot -a -f %s")
          (const :tag "scrot" "scrot -s %s")
          (const :tag "gm" "gm import %s")
          (const :tag "imagemagick/import" "import %s")
          (const :tag "imagemagick/import + xclip to save to clipboard"
                 "export filename=\"%s\"; import png:\"$filename\" ;xclip -selection clipboard -target image/png -filter < \"$filename\" &>/dev/null")
          (const :tag "xfce4-screenshooter" "xfce4-screenshooter -r -o cat > %s")
          ;; screenshot method in ms-windows, /capture=4 stands for interactive.
          (const :tag "IrfanView" "i_view64 /capture=4 /convert=\"%s\"")
          ;; screenshot script in osx, -i stands for interactive,
          ;; press space key to toggle between selection and
          ;; window/application mode.
          (const :tag "screencapture" "screencapture -i %s")
          ;; take an image that is already on the clipboard, for Linux
          (const :tag "xclip"
                 "xclip -selection clipboard -t image/png -o > %s")
          ;; take an image that is already on the clipboard, for Windows
          (const :tag "imagemagick/convert" "convert clipboard: %s")
          (function :tag "Custom function"))
  :group 'pix2tex)

(defcustom pix2tex-screenshot-file-basename
  (expand-file-name "pix2tex" temporary-file-directory)
  "The file to capture pix2tex screenshots to."
  :type 'string
  :group 'pix2tex)

(defvar pix2tex--process-alist nil)

(defun pix2tex--sentinel (process _status)
  "Process sentinel for pix2tex.

Inset the converted LaTeX code.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (when (eq (process-status process) 'exit)
      (with-current-buffer proc-buf
        (goto-char (point-min))
        (let* ((info (alist-get process pix2tex--process-alist))
               (file (car info))
               (pos (cdr info))
               (beg (search-forward (concat file ": ") (pos-eol) t))
               (end (re-search-forward "^$" nil t))
               (result (buffer-substring-no-properties beg (1- end))))
          (with-current-buffer (marker-buffer pos)
            (save-excursion
              (goto-char pos)
              (insert result)))
          (delete-file file)
          (setf (alist-get process pix2tex--process-alist nil 'remove) nil)
          (kill-buffer proc-buf))))))

(defun pix2tex-request (file pos)
  "Start a pix2tex process."
  (let* ((args `("file" ,file))
         (process (apply #'start-process
                         "pix2tex"
                         (generate-new-buffer "*pix2tex*")
                         "pix2tex"
                         args)))
    (with-current-buffer (process-buffer process)
      (set-process-query-on-exit-flag process nil)
      (setf (alist-get process pix2tex--process-alist) (cons file pos))
      (set-process-sentinel process #'pix2tex--sentinel))))

;;;###autoload
(defun pix2tex-screenshot ()
  "Capture screenshot and send result to pix2tex."
  (interactive)
  ;;; taken from https://github.com/jethrokuan/mathpix.el
  (let ((default-directory "~")
        (orig-pos (point-marker))
        (file-name (make-temp-file
                    pix2tex-screenshot-file-basename
                    nil ".png")))
    (unless (file-exists-p (file-name-directory file-name))
      (make-directory (file-name-directory file-name) t))
    (if (functionp pix2tex-screenshot-method)
        (funcall pix2tex-screenshot-method file-name)
      (shell-command-to-string
       (format pix2tex-screenshot-method file-name)))
    (when (file-exists-p file-name)
      (pix2tex-request (expand-file-name file-name) orig-pos))))

(provide 'pix2tex)
;;; pix2tex.el ends here
