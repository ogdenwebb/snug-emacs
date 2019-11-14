;; Wgrep
(use-package ag
  :defer t
  :if (executable-find "ag")
  :commands (ag ag-dired ag-dired-regexp ag-files ag-project ag-regexp
                ag-project-files ag-project-at-point)
  :config
  (setq ag-highlight-search t
        ag-reuse-buffers t
        ag-reuse-window t))

(use-package wgrep
  :defer t
  :commands (wgrep wgrep-setup wgrep-change-to-wgrep-mode)
  :config)

(use-package wgrep-ag
  :after wgrep)

(provide 'use-grep)
