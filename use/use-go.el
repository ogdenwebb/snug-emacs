;; Support for golang -*- lexical-binding: t -*-

(use-package go-mode
  :mode (("\\.go\\'" . go-ts-mode)
         ("\\.go\\.mod" . go-mod-ts-mode))
  ;; :hook (lsp-deferred)
  :interpreter "go"
  :config
  )

(use-package gotest-ts
  :if (featurep 'treesit)
  :hook (go-ts-mode . gotest-ts-setup))

(use-package go-tag
  :commands (go-tag-add go-tag-remove go-tag-refresh))

(use-package gorepl-mode
  :hook (go-mode go-ts-mode))

(use-package go-gen-test
  :after go-mode)

(provide 'use-go)
