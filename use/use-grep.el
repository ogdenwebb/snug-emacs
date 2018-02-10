;; Wgrep
(use-package wgrep
  :defer t
  :config
  (use-package wgrep-ag
    :after wgrep))

(provide 'use-grep)
