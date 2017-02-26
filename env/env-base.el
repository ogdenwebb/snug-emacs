(defun byte-compile-config ()
  (interactive)
  ;; (async-byte-recompile-directory))
  (mapc (lambda (fname) (byte-compile-file (concat user-emacs-directory "/" fname)))
        '("init.el"
          "env/env-modeline.el"))

  (message "Success!"))

(setq byte-compile--use-old-handlers nil)
(setq load-prefer-newer t)

;; Use Common Lisp
(use-package cl)

(when window-system
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;; Load my configuration files
(add-to-list 'load-path "~/.emacs.d/env/")
(add-to-list 'load-path "~/.emacs.d/use/")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/kaolin-theme/")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(provide 'env-base)
