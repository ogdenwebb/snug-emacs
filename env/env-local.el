(use-package i3wm-config-mode
  :straight nil
  :mode (("[/\\]\\.config/i3/config$" . i3wm-config-mode)
         ("[/\\]\\.i3/config$" . i3wm-config-mode))
  :load-path "dev/i3wm-Config-Mode")

(provide 'env-local)
