;; Lua language support -*- lexical-binding: t -*-

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  :interpreter "lua"
  :config
  (setq-default lua-indent-level 4))

;; TODO rework
(use-package company-lua
             :disabled t
  :after (lua-mode company))

(use-package moonscript
  :mode ("\\.moon$" . moonscript-mode))

(provide 'use-lua)
