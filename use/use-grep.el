;; Wgrep
(use-package wgrep
  :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :config)

(use-package wgrep-ag
  :after wgrep)

(provide 'use-grep)
