(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

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

(straight-use-package
     '(font-lock+ :type git :host github :repo "emacsmirror/font-lock-plus"))

(when window-system
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;; Load my configuration files
(add-to-list 'load-path "~/.emacs.d/env/")
(add-to-list 'load-path "~/.emacs.d/use/")
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; Needed by emacs without daemon
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/kaolin-theme/themes/")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(provide 'env-boot)
