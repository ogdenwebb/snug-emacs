;; Snippets -*- lexical-binding: t -*-
(use-package yasnippet
  :diminish yas-minor-mode
  :defer t
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets))

(provide 'use-yasnippet)
