;;; Evil mode -*- lexical-binding: t -*-
;; TODO: (??)  set shift-select-mode to nil

(use-package evil
  ;; :defer .1
  :hook (elpaca-after-init . evil-mode)
  :preface
  (setq evil-want-integration t ;; required by evil-collection
        evil-want-keybinding nil

        ;; evil-ex-complete-emacs-commands nil
        ;; evil-vsplit-window-right t ;; like vim's 'splitright'
        ;; evil-split-window-below t ;; like vim's 'splitbelow'


        evil-want-fine-undo t
        evil-want-Y-yank-to-eol t
        evil-ex-search-vim-style-regexp t
        ;; C-u is universal-argument in Emacs
        evil-want-C-u-scroll nil
        ;; evil-ex-search-persistent-highlight nil

        evil-respect-visual-line-mode t

        evil-visual-state-cursor 'hollow
        evil-mode-line-format 'nil

        evil-ex-search-case 'smart
        evil-ex-substitute-case t
        evil-move-beyond-eol t
        evil-shift-round nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        evil-insert-skip-empty-lines t

        evil-magic t
        ;; evil-magic 'very-magic
        ;; evil-vim-regexp-replacements nil

        ;; evil-auto-indent nil
        evil-indent-convert-tabs t
        )

  :config
  (defun snug-evil/declare-repeat (&rest commands)
    "TODO"
    (dolist (cmd commands)
      (evil-declare-repeat cmd)))


  (setq evil-shift-width snug-default-indent-width)

  (evil-select-search-module 'evil-search-module 'evil-search)
  (add-hook 'evil-insert-state-entry-hook #'evil-ex-nohighlight)

  (defun evil-clear-hl-after-search ()
    "Clear evil active highlighting."
    (interactive)
    (when evil-ex-active-highlights-alist
      (unless (memq last-command '(evil-search-next
                                   evil-search-previous
                                   evil-ex-search-next
                                   evil-ex-search-previous))
        (evil-ex-nohighlight))))

  (add-hook 'post-command-hook #'evil-clear-hl-after-search)

  ;; Initial states
  (evil-set-initial-state 'nrepl-mode 'insert)

  ;; Evil ex commands
  ;; (evil-ex-define-cmd "pu[pgrade]" 'package-upgrade-all)
  ;; (evil-ex-define-cmd "pi[stall]"  'package-install)
  ;; (evil-ex-define-cmd "pf[orce]"   'package-reinstall)
  ;; (evil-ex-define-cmd "pd[elete]"  'package-delete)
  ;; (evil-ex-define-cmd "pc[lean]"   'package-autoremove)
  (evil-ex-define-cmd "lt"         'load-theme)

  ;; Enable to use C-o/C-i after these commands
  ;; TODO: macro def-evil-jump or smth
  (with-eval-after-load 'evil
    ;; (snug-evil/declare-repeat 'puni-slurp-forward 'puni-slurp-backward)


    ;; (evil-add-command-properties #'org-footnote-new   :jump t)
    (evil-add-command-properties #'org-footnote-action :jump t)
    ;; (evil-add-command-properties #'org-mark-ring-push :jump t)
    ;; (evil-add-command-properties #'org-ctrl-c-ctrl-c  :jump t)
    (evil-add-command-properties #'find-file-at-point :jump t)
    (evil-add-command-properties #'swiper             :jump t)
    (evil-add-command-properties #'counsel-rg         :jump t)
    (evil-add-command-properties #'counsel-recentf    :jump t)
    (evil-add-command-properties #'counsel-ag         :jump t)
    (evil-add-command-properties #'godef-jump         :jump t)
    (evil-add-command-properties #'counsel-fzf        :jump t))
  ;; (evil-mode t)
  )

;; Vim-like keybindings everywhere in Emacs
(use-package evil-collection
  :after evil
  :config
  (setq-default evil-collection-company-use-tng nil)
  (setq evil-collection-mode-list (remove 'lispy evil-collection-mode-list))
  (setq evil-collection-mode-list (remove 'corfu evil-collection-mode-list))
  (evil-collection-init))

(use-package evil-commentary
  ;; :after evil
  :commands (evil-commentary evil-commentary-yank))


;; TEST SUITE

;; (use-package evil-goggles
;;   ;; :requires evil
;;   :disabled t
;;   :hook (elpaca-after-init . evil-goggles-mode)
;;   :config
;;   (setq evil-goggles-duration 0.1
;;         evil-goggles-enable-delete nil
;;         evil-goggles-pulse t))


;; ;; TODO: maps
(use-package evil-surround
  ;; :after evil
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))

(use-package evil-matchit
  ;; :after evil
  ;; :commands (evilmi-jump-items evilmi-text-object global-evil-matchit-mode)
  :hook (evil-mode . global-evil-matchit-mode))

(use-package evil-visualstar
  :commands (global-evil-visualstar-mode
             evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :config
  (global-evil-visualstar-mode))

(use-package evil-lion
  :commands (evil-lion-mode evil-lion-left evil-lion-right)
  :config (setq evil-lion-squeeze-spaces t))

;; ;; Folding
;; ;; TODO: maps
;; (use-package evil-vimish-fold
;;   :disabled t
;;   :hook (elpaca-after-init . evil-vimish-fold-mode)
;;   :commands evil-vimish-fold-mode
;;   :config
;;   (setq vimish-fold-header-width nil))

;; ;; C-a C-x from vim to inc/dec numbers
(use-package evil-numbers
  ;; :after evil
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

(use-package evil-args
  ;; :after evil
  :commands (evil-inner-arg evil-outer-arg
                            evil-forward-arg evil-backward-arg
                            evil-jump-out-args)
  :general
  (general-itomap "a" 'evil-inner-arg)
  (general-otomap "a" 'evil-outer-arg))


;; ;; gr operator, like vim's ReplaceWithRegister
(use-package evil-replace-with-register
  ;; :after evil
  :general
  (general-nvmap "gr" 'evil-replace-with-register))

;; ;; Easy text exchange operator for Evil.
(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-cx-install))

(use-package evil-embrace
  :after evil-surround
  :config
  ;; (add-hook 'LaTeX-mode-hook #'embrace-LaTeX-mode-hook)
  ;; (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (setq evil-embrace-show-help-p nil)
  (evil-embrace-enable-evil-surround-integration))

(use-package vdiff
  :disabled t
  :commands (vdiff-buffers vdiff-files vdiff-buffers3 vdiff-current-file vdiff-merge-conflict))

(use-package evil-snipe
  :disabled t
  :after evil
  :config
  (evil-snipe-mode t)
  (evil-snipe-override-mode t))

(use-package evil-tex
  :disabled t)

;; Evil XML Attributes Text Object
(use-package exato
  :after evil)

;; Better indent textobjects for evil
(use-package evil-indent-plus
  :after evil)

(provide 'env-evil)
