;;; Rust programming language -*- lexical-binding: t -*-

(use-package rustic
  :config
  (setq rustic-format-on-save t)
  (setq rustic-lsp-client 'eglot))

(provide 'use-rust)
