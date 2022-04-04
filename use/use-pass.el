;;; Password-store support  -*- lexical-binding: t -*-

(setq auth-source-debug t)

;; (with-eval-after-load 'magit
;;   (setq magit-process-find-password-functions '(magit-process-password-auth-source))
;;   )

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

(use-package password-store
  :config
  (setq password-store-time-before-clipboard-restore 30))

(provide 'use-pass)
