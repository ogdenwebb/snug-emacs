;; Increase garbage collection for speedup
(setq-default gc-cons-threshold 20000000 ; or even 1000000000
              gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)))

;; TODO:
(defun byte-compile-config ()
  (interactive)
  ;; (async-byte-recompile-directory))
  (byte-compile-file (concat user-emacs-directory "/" "init.el") 0)
  (byte-recompile-directory (concat user-emacs-directory "/env") 0)
  ;; (mapc (lambda (fname) (byte-compile-file (concat user-emacs-directory "/" fname)))
  ;;       '("init.el"
  ;;         "env/env-boot.el"
  ;;         "env/env-common.el"
  ;;         "env/env-modeline.el"))

  (message "Success!"))

(setq byte-compile--use-old-handlers nil)
(setq load-prefer-newer t)

;; start scratch in text mode (usefull to get a faster Emacs load time
;; because it avoids autoloads of elisp modes)
(setq initial-major-mode 'text-mode)

;; Use Common Lisp library
(use-package cl)

(when window-system
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;; Load my configuration files
(add-to-list 'load-path "~/.emacs.d/env/")
(add-to-list 'load-path "~/.emacs.d/use/")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/modeline/")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(provide 'env-boot)
