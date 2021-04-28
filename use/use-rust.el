;;; Rust programming language -*- lexical-binding: t -*-

(use-package rust-mode
  :mode "\\.rs$")

(use-package racer
  :hook (rust-mode . racer-mode)
  :config
  (add-hook 'rust-mode-hook #'eldoc-mode))

(use-package company-racer
  :after (company racer)
  (add-to-list 'company-backends 'company-racer))

(provide 'use-rust)
