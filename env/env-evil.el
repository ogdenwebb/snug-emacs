;;; Evil mode -*- lexical-binding: t -*-
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil ;; required by evil-collection
        evil-want-fine-undo t
        evil-want-Y-yank-to-eol t
        evil-ex-search-vim-style-regexp t
        ;; evil-ex-search-persistent-highlight nil

        evil-ex-search-case 'sensitive
        evil-ex-substitute-case t
        evil-move-beyond-eol t
        evil-shift-round nil
        evil-symbol-word-search t
        evil-insert-skip-empty-lines t

        ;; evil-magic t
        ;; evil-magic 'very-magic
        ;; evil-vim-regexp-replacements nil

        ;; evil-auto-indent nil
        evil-indent-convert-tabs t)

  :config

  (evil-select-search-module 'evil-search-module 'evil-search)
  (add-hook 'evil-insert-state-entry-hook #'evil-ex-nohighlight)

  ;; TODO: check if higlight is active
  (defun evil-clear-hl-after-search ()
    (unless (memq last-command '(evil-search-next
                                 evil-search-previous
                                 evil-ex-search-next
                                 evil-ex-search-previous))
      (evil-ex-nohighlight)))

  (add-hook 'post-command-hook #'evil-clear-hl-after-search)

  ;; Initial states
  (evil-set-initial-state 'nrepl-mode 'insert)

  ;; fix for company
  (evil-declare-change-repeat 'company-complete)

  ;; Evil ex
  ;; (evil-ex-define-cmd "pu[pgrade]" 'package-utils-upgrade-all)
  (evil-ex-define-cmd "pu[pgrade]" 'package-upgrade-all)
  (evil-ex-define-cmd "pi[stall]"  'package-install)
  (evil-ex-define-cmd "pd[elete]"  'package-delete)
  (evil-ex-define-cmd "lt"  'load-theme)

  (evil-mode 1))

;; Vim-like keybindings everywhere in Emacs
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (setq evil-goggles-duration 0.1
        evil-goggles-enable-delete nil
        evil-goggles-pulse t)

  (add-hook 'after-init-hook 'evil-goggles-mode))

(use-package evil-commentary
  :ensure t
  :after evil
  :commands (evil-commentary evil-commentary-yank))

; TODO: maps
(use-package evil-surround
  :ensure t
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))

;; Magit
(use-package evil-magit
  :ensure t
  :after magit)

(use-package evil-matchit
  :ensure t
  :commands (evilmi-jump-items evilmi-text-object global-evil-matchit-mode)
  :config
  (global-evil-matchit-mode t))

(use-package evil-visualstar
  :ensure t
  :commands (global-evil-visualstar-mode
             evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :config
  (global-evil-visualstar-mode))

(use-package evil-lion
 :ensure t
 :commands (evil-lion-mode evil-lion-left evil-lion-right)
 :config (setq evil-lion-squeeze-spaces t))

;; Folding
;; TODO: maps
(use-package evil-vimish-fold
  :ensure t
  :commands evil-vimish-fold-mode
  :config
  (add-to-list 'after-init-hook #'evil-vimish-fold-mode)
  (setq vimish-fold-header-width nil))

(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :ensure t)

(use-package evil-args
  :after evil
  :commands (evil-inner-arg evil-outer-arg
             evil-forward-arg evil-backward-arg
             evil-jump-out-args)
  :general
  (general-itomap "a" 'evil-inner-arg)
  (general-otomap "a" 'evil-outer-arg))

(provide 'env-evil)
