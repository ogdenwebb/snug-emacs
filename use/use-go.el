(use-package go-mode
  :mode (("\\.go\\'" . go-mode)))

(use-package company-go
  :after (company go-mode)
  :config
  (setq company-go-show-annotation t))

(provide 'use-go)
