(use-package nim-mode
  :mode (("\\.nim\\'" . nim-mode)
         ("\\.nims\\'" . nimscript-mode))
  :config
  (add-hook 'nim-mode-hook 'nimsuggest-mode))

(provide 'use-nim)
