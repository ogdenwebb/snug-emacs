;; Snippets -*- lexical-binding: t -*-
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  ;; (setq yas-verbosity nil
  ;;       yas-also-auto-indent-first-line t
  ;;       yas-wrap-around-region t)
  (use-package yasnippet-snippets))

(provide 'use-yasnippet)
