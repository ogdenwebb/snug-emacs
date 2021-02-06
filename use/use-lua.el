;; Lua language support -*- lexical-binding: t -*-

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  :interpreter "lua"
  :config
  (setq-default lua-indent-level 4))

(use-package company-lua
  :after (lua-mode company))

(provide 'use-lua)
