(use-package awesome-tab
  :disabled t
  :load-path "dev/awesome-tab"
  :config
  (awesome-tab-mode t)
)

(use-package powerline
  :defer t)

(use-package centaur-tabs
  ;; :requires (powerline)
  ;; :disabled t
  :straight (:host github :repo "ema2159/centaur-tabs")
  :hook (after-init . centaur-tabs-mode)
  :config
  (setq centaur-tabs-background-color (face-background 'centaur-tabs-default)
        centaur-tabs-set-close-button nil
        centaur-tabs-style "wave"

        ;; centaur-tabs-set-bar 'left
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        ;; centaur-tabs-gray-out-icons 'buffer
        ;; centaur-tabs-plain-icons t)
        ;; centaur-tabs-set-modified-marker t)
        )
  )

(provide 'use-tabs)
