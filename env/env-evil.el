;; Enable evil mode
(use-package evil
  :ensure t
  :config
  (setq evil-want-fine-undo nil)

  ;; Initial states
  (evil-set-initial-state 'nrepl-mode 'insert)

  (evil-mode 1)
  (evil-commentary-mode))

  ;; Set default states

  ;; (add-hook 'monky-mode-hook
  ;;        (lambda ()
  ;;          (evil-add-hjkl-bindings monky-mode-map 'emacs
  ;;            (kbd "/")       'evil-search-forward
  ;;            (kbd "n")       'evil-search-next
  ;;            (kbd "N")       'evil-search-previous
  ;;            (kbd "C-d")     'evil-scroll-down
  ;;            (kbd "C-u")     'evil-scroll-up
  ;;            (kbd "C-w C-w") 'other-window)))

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

(provide 'env-evil)
