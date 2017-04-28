;; Enable evil mode
(use-package evil
  :ensure t
  :config
  (setq evil-want-fine-undo nil)
  ;; (setq evil-auto-indent nil)

  ;; Initial states
  (evil-set-initial-state 'nrepl-mode 'insert)

  ;; fix for company
  (evil-declare-change-repeat 'company-complete)

  ;; Evil ex
  (evil-ex-define-cmd "pu[pgrade]" 'package-utils-upgrade-all)

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

(provide 'env-evil)
