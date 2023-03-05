;; Snippets -*- lexical-binding: t -*-

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (elpaca-after-init . yas-global-mode)
  :config
  ;; (setq yas-verbosity nil
  ;;       yas-also-auto-indent-first-line t
  ;;       yas-wrap-around-region t)
)

(provide 'use-yasnippet)
