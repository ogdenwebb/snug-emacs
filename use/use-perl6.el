(use-package perl6-detect)

(use-package flycheck-perl6
  :after perl6-mode
  :config (add-hook 'perl6-mode-hook #'flycheck-mode))

(provide 'use-perl6)
