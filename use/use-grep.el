;; Wgrep
(use-package ag
  :if (executable-find "ag")
  :config
  (setq ag-highlight-search t
        ag-reuse-buffers t
        ag-reuse-window t))

(use-package wgrep
  :defer t
  ;; :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :config)

(use-package wgrep-ag
  :after wgrep)

(provide 'use-grep)
