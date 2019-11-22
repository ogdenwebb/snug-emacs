;;; Snug -*- lexical-binding: t -*-

;; Setup debug mode
(eval-and-compile
  (defvar snug-debug-mode
    (or (getenv "DEBUG") init-file-debug)
    "Debug mode, enable through DEBUG=1 or use --debug-init.")
  (setq debug-on-error (and (not noninteractive) snug-debug-mode)))

;; Disable certain byte compiler warnings to cut down on the noise.
(unless snug-debug-mode
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)))

;; Startup & package manager
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil
      byte-compile--use-old-handlers nil
      load-prefer-newer t
      auto-window-vscroll nil
      site-run-file nil
      package-enable-at-startup nil)

;; Increase garbage collection for speedup
(setq-default gc-cons-threshold 100000000
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


;; Security settings
(setq gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      use-package-enable-imenu-support t)

;; Display use-package debug stuff when debug-on-error is t
(if snug-debug-mode
      (setq use-package-expand-minimally nil
            use-package-verbose t
            use-package-compute-statistics t)
  (setq use-package-expand-minimally t
        use-package-verbose nil))

;; (eval-when-compile (require 'subr-x))
(use-package subr-x
  :straight nil
  :defer t)

(use-package git
  :defer t)

;; Help keeping ~/.emacs.d clean
(use-package no-littering)

;; TODO: Generate autoloads

;; Define directories
(eval-and-compile
  (defvar home-directory (getenv "HOME")
    "User $HOME.")

  (defvar snug-root (file-truename user-emacs-directory)
    "Root of the snug.")

  (defvar snug-dir (concat snug-root "snug/")
    "The main directory of snug-emacs configuration.")

  ;; (defvar snug-cache-dir
  ;;   (if (getenv "XDG_DATA_HOME")
  ;;       (concat (getenv "XDG_DATA_HOME") "/emacs/")
  ;;     (expand-file-name "~/.local/share/emacs/"))
  ;;   "Directory for data.")

  ;; (defvar snug-cache-dir
  ;;   (if (getenv "XDG_CACHE_HOME")
  ;;       (concat (getenv "XDG_CACHE_HOME") "/emacs/")
  ;;     (expand-file-name "~/.cache/emacs/"))
  ;;   "Directory for cache.")
  )

;; Set initial mode to text-mode
(setq initial-major-mode 'fundamental-mode)

;; Use Common Lisp library
(use-package cl-lib :defer t)

;; Add configuration directories to `load-path'
(setq load-path (append '("~/.emacs.d/snug/" "~/.emacs.d/env/"
                           "~/.emacs.d/use/" "~/.emacs.d/user/"
                           "~/.emacs.d/completion/" "~/.emacs.d/modeline/")
                         load-path))

;; Custom file
;; TODO: add no-littering dir
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file t t))

;; Server
(use-package server
  :straight nil
  :hook (after-init . server-mode))

;; Here we go
(defmacro snug/init (&rest body)
  (declare (indent defun))
  (add-to-list 'body 'env-fu t)
  (dolist (pkg body)
    (require pkg nil t)))

;; Use a hook so the message doesn't get clobbered by other messages.
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs ready in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))


;; Use git version of Org-mode
;; https://github.com/raxod502/straight.el#installing-org-with-straightel
(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)
(straight-use-package 'org)

;; Text icons
(use-package all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
                                   all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon
                                   all-the-icons-install-fonts)
  :config
  (setq all-the-icons-scale-factor 1.0))

(require 'snug-core)
(require 'snug-settings)

(provide 'snug)
