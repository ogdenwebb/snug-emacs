(use-package smartparens
  :init
  (add-hook 'after-init-hook #'smartparens-global-mode)
  :config
  (use-package smartparens-config)
  ;; (setq sp-autowrap-region nil ; let evil-surround handle this
  (setq sp-highlight-pair-overlay nil
        sp-cancel-autoskip-on-backward-movement nil)

  (setq-default sp-autoskip-closing-pair 'always-end)

  ;; disable smartparens in evil-mode's replace state (they conflict)
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook #'turn-on-smartparens-mode))

(provide 'use-sp)
