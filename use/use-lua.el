;; Lua language support -*- lexical bindings: t -*-

(use-package lua-mode
  :defer t
  :mode ("\\.lua$" . lua-mode)
  :interpreter "lua"
  :config
  (setq-default lua-indent-level tab-width))

(use-package company-lua
  :defer t
  :after (lua-mode company))

(provide 'use-lua)
