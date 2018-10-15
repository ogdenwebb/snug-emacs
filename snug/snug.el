;;; -*- lexical-binding: t -*-

;; Disable certain byte compiler warnings to cut down on the noise.
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil
      byte-compile--use-old-handlers nil
      load-prefer-newer t
      package-enable-at-startup nil   ; To prevent initialising twice
      package-user-dir (concat user-emacs-directory "elpa")
      package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/"))
      ;; don't add that `custom-set-variables' block to init
      package--init-file-ensured t)

;; Default directories

;; Increase garbage collection for speedup
(setq-default gc-cons-threshold 20000000 ; or even 1000000000
              gc-cons-percentage 0.6
              package-enable-at-startup nil
              message-log-max 16384
              auto-window-vscroll nil)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq file-name-handler-alist file-name-handler-alist-old
                    gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)
              (garbage-collect)) t)

;; Print log while loading
;; (setq use-package-verbose t)
;; Add the macro generated list of `package.el' loadpaths to `load-path'.
(mapc #'(lambda (add) (add-to-list 'load-path add))
  (eval-when-compile
    ;; (require 'package)
    (package-initialize)
    ;; Install use-package if not installed yet.
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (let ((package-user-dir-real (file-truename package-user-dir)))
      ;; The reverse is necessary, because outside we mapc
      ;; add-to-list element-by-element, which reverses.
      (nreverse (apply #'nconc
           ;; Only keep package.el provided loadpaths.
           (mapcar #'(lambda (path)
                   (if (string-prefix-p package-user-dir-real path)
                   (list path)
                     nil))
               load-path))))))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

(with-eval-after-load "info"
  (info-initialize)
  (dolist (dir (directory-files package-user-dir))
    (let ((fdir (concat (file-name-as-directory package-user-dir) dir)))
      (unless (or (member dir '("." ".." "archives" "gnupg"))
                  (not (file-directory-p fdir))
                  (not (file-exists-p (concat (file-name-as-directory fdir) "dir"))))
        (add-to-list 'Info-directory-list fdir)))))

;; Help keeping ~/.emacs.d clean
(use-package no-littering)

;; TODO: Generate autoloads

;; Define directories
(setq home-directory (getenv "HOME"))
(defvar snug-dir (file-truename user-emacs-directory)
  "Root of the snug configuration.")

;; Start scratch in text mode
(setq initial-major-mode 'text-mode)

;; Use Common Lisp library
(require 'cl-lib)

;; Add configuration directories to `load-path'
(add-to-list 'load-path "~/.emacs.d/snug/")
(add-to-list 'load-path "~/.emacs.d/env/")
(add-to-list 'load-path "~/.emacs.d/use/")
(add-to-list 'load-path "~/.emacs.d/completion/")
(add-to-list 'load-path "~/.emacs.d/modeline/")

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t t)

(when window-system
  (require 'server)
  (unless (server-running-p)
    (server-start)))

(defmacro snug/init (&rest body)
  (declare (indent defun))
  (add-to-list 'body 'env-fu t)
  (dolist (pkg body)
    (require pkg)))

;; Use a hook so the message doesn't get clobbered by other messages.
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs ready in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))

(require 'snug-settings)

(provide 'snug)
