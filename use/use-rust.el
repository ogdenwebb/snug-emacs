;;; Rust programming language -*- lexical-binding: t -*-

(use-package rustic
  :config
  (setq rustic-format-on-save t
        rustic-lsp-client 'eglot)
  )

(provide 'use-rust)
