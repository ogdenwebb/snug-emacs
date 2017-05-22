;; Enable evil mode
(use-package evil
  :ensure t
  :config
  (setq evil-want-fine-undo nil)

  ;; (setq evil-search-module 'evil-search)
  ;; (setq evil-magic 'very-magic)
  ;; (setq evil-vim-regexp-replacements nil)
  (setq evil-ex-search-vim-style-regexp t)
  (setq evil-move-beyond-eol t)
  ;; (setq evil-auto-indent nil)

  ;; Initial states
  (evil-set-initial-state 'nrepl-mode 'insert)

  ;; fix for company
  (evil-declare-change-repeat 'company-complete)

  ;; Evil ex
  (evil-ex-define-cmd "pu[pgrade]" 'package-utils-upgrade-all)
  (evil-ex-define-cmd "pi[stall]" 'package-install)
  (evil-ex-define-cmd "pd[elete]" 'package-delete)

  (evil-mode 1))

(use-package evil-commentary
  :after evil
  :commands (evil-commentary evil-commentary-yank)
  :general
  (general-nvmap
    "gc" 'evil-commentary
    "gy" 'evil-commentary-yank))

(use-package evil-surround
  :ensure t
  :init
  (global-evil-surround-mode 1))

;; Magit
(use-package evil-magit
  :after magit)

(use-package evil-matchit
  :config
  (global-evil-matchit-mode t))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

(use-package evil-lion
  :commands (evil-lion-mode evil-lion-left evil-lion-right)
  :config
  (setq evil-lion-squeeze-spaces t)
  :general
  (general-nvmap
   "ga"  'evil-lion-left
   "gA"  'evil-lion-right))

;; (use-package evil-goggles
;;   :load-path "dev/evil-goggles"
;;   :config
;;   (evil-goggles-mode))

;; Folding

(use-package vimish-fold
  :config
  (use-package evil-vimish-fold
    :init
    (evil-vimish-fold-mode 1))
  (setq vimish-fold-header-width nil))


(provide 'env-evil)
