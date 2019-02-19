;; Lua language support -*- lexical bindings: t -*-

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  :interpreter "lua"
  :init
  (setq-default lua-indent-level tab-width))

(use-package company-lua
  :after (lua-mode company))

(provide 'use-lua)
