;;; Snug -*- lexical-binding: t -*-

;; Setup debug mode
(eval-and-compile
  (defvar snug-debug-mode
    (or (getenv "DEBUG") init-file-debug)
    "Debug mode, enable through DEBUG=1 or use --debug-init.")
  (setq debug-on-error (and (not noninteractive) snug-debug-mode)
        jka-compr-verbose snug-debug-mode))

;; TODO: let user setup this
(defvar snug-verbose-byte-compile-warnings nil)

;; Disable certain byte compiler warnings to cut down on the noise.
(if (or snug-debug-mode snug-verbose-byte-compile-warnings)
    (setq byte-compile-warnings t)
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)))

;; Startup & package manager
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      byte-compile--use-old-handlers nil
      load-prefer-newer t
      ;; Don't load site packages
      site-run-file nil)

;; Advanced logging
;; (setq-default message-log-max 16384)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq file-name-handler-alist file-name-handler-alist-old)))
;;                     gc-cons-threshold 16777216
;;                     gc-cons-percentage 0.1)
;;               (garbage-collect)) t)

;; Ensure `snug' is in `load-path'
(add-to-list 'load-path (file-name-directory load-file-name))

;; Require necessary snug things
(require 'snug-core)
(require 'snug-pack)

;; Security settings
;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be at
;; least a little more discerning.
(setq gnutls-verify-error (not (getenv-internal "INSECURE"))
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not (version< emacs-version "26.3"))
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
  ;; Emacs is built with `gnutls' by default, so `tls-program' would not be
  ;; used in that case. Otherwise, people have reasons to not go with
  ;; `gnutls', we use `openssl' instead. For more details, see
  ;; https://redd.it/8sykl1
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))

;; Display use-package debug stuff when debug-on-error is t
(if snug-debug-mode
    (setq use-package-expand-minimally nil
          use-package-verbose t
          use-package-compute-statistics t
          message-log-max t
          use-package-minimum-reported-time 0.01)
  (setq use-package-expand-minimally t
        use-package-verbose nil))

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

;; Don't load any other files besides this config
(setq inhibit-default-init t
      ;; Set initial mode to text-mode instead of elisp
      initial-major-mode 'fundamental-mode)

;; Use Common Lisp library
(use-package cl-lib
  :ensure nil
  :defer t)

;; Add configuration directories to `load-path'
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("snug" "env" "use" "user" "completion" "env/modeline"))
    (cl-pushnew (expand-file-name dir snug-root) load-path)))

(update-load-path)

(require 'env-maps)
(elpaca-wait)

(use-package gcmh
  :hook (elpaca-after-init . gcmh-mode)
  :config
  (setq gcmh-verbose             nil
        ;; gcmh-low-cons-threshold  #x800000
        ;; gcmh-high-cons-threshold #x800000
        gcmh-idle-delay          5 ;; old is 300

        ;; Don’t compact font caches during GC.
        inhibit-compacting-font-caches t
        gc-cons-percentage 0.1))

;; (use-package subr-x :ensure nil)

;; Help keeping emacs directory clean

(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless *IS-MAC*   (setq command-line-ns-option-alist nil))
(unless *IS-LINUX* (setq command-line-x-option-alist nil))

;; Custom file
;; TODO: add no-littering dir
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file t t))

;; Server
(use-package server
  :ensure nil
  ;; :hook (elpaca-after-init . server-mode)
  :config
  (unless (server-running-p)
    server-mode)
  (add-hook 'server-done-hook 'recentf-cleanup))

;; Here we go
(defmacro snug/init (&rest body)
  (declare (indent defun))
  (let ((gc-cons-threshold most-positive-fixnum))
    (add-to-list 'body 'env-fun t)
    (dolist (pkg body)
      (require pkg nil t))))

;; Text icons
(use-package all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
                                   all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon
                                   all-the-icons-install-fonts)
  :config
  (setq all-the-icons-scale-factor 1.0))

(require 'snug-settings)

(provide 'snug)
