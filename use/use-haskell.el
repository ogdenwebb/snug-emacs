(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (add-hook 'haskell-mode-hook 'haskell-setup)
  (add-hook 'haskell-cabal-mode-hook 'haskell-setup)

  (add-hook 'haskell-mode-hook (lambda () (setq electric-indent-inhibit t))))

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode))

(provide 'use-haskell)
