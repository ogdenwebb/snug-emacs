;; Silver Searcher aka ag

(defvar snug-default-grep "rg"
  "")

(use-package ag
  :if (executable-find "ag")
  :commands (ag ag-dired ag-dired-regexp ag-files ag-project ag-regexp
                ag-project-files ag-project-at-point)
  :config
  (setq ag-highlight-search t
        ag-reuse-buffers t
        ag-reuse-window t))

;; Ripgrep
(use-package rg
  :if (executable-find "rg")
  :commands (rg rgrep rg-menu rg-dwim)
  :config
  (setq rg-group-result t
        rg-hide-command t
        rg-show-columns nil
        rg-show-header t
        rg-custom-type-aliases nil
        rg-default-alias-fallback "all"))

;; Writable grep buffer
(use-package wgrep
  :commands (wgrep wgrep-setup wgrep-change-to-wgrep-mode)
  :config)

(use-package wgrep-ag
  :after wgrep)

(provide 'use-grep)
