;;; -*- lexical-binding: t -*-

;; Disable certain byte compiler warnings to cut down on the noise.
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Startup & package manager
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil
      byte-compile--use-old-handlers nil
      load-prefer-newer t)

;; Increase garbage collection for speedup
(setq-default gc-cons-threshold 20000000
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

;; Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      use-package-expand-minimally (if debug-on-error nil t))

;; Help keeping ~/.emacs.d clean
(use-package no-littering)

;; TODO: Generate autoloads

;; Define directories
(defvar home-directory (getenv "HOME")
  "User $HOME.")

(defvar snug-root (file-truename user-emacs-directory)
  "Root of the snug.")

(defvar snug-dir (concat snug-root "snug/")
  "The main directory of snug-emacs configuration.")
;; Start initial mode to text-mode
(setq initial-major-mode 'text-mode)

;; Use Common Lisp library
(require 'cl-lib)

;; Add configuration directories to `load-path'
(add-to-list 'load-path "~/.emacs.d/snug/")
(add-to-list 'load-path "~/.emacs.d/env/")
(add-to-list 'load-path "~/.emacs.d/use/")
(add-to-list 'load-path "~/.emacs.d/user/")
(add-to-list 'load-path "~/.emacs.d/completion/")
(add-to-list 'load-path "~/.emacs.d/modeline/")

;; Custom file
;; TODO: add no-littering dir
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file t t))

;; Server
(when window-system
  (require 'server)
  (unless (server-running-p)
    (server-start)))

(defun keyword-to-name-str (keyword)
  "Return KEYWORD symbol without initial color as string
i.e. :keyword to \"keyword\"."
  (substring (symbol-name keyword) 1))

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

(require 'snug-core)
(require 'snug-settings)

(provide 'snug)
