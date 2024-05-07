;;; Password-store support  -*- lexical-binding: t -*-

(use-package auth-source-pass
  :ensure nil
  :config
  (auth-source-pass-enable))

(use-package password-store
  :config
  (setq password-store-time-before-clipboard-restore 30))

(provide 'use-pass)
