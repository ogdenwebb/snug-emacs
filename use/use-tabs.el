(use-package awesome-tab
  :disabled t
  :load-path "dev/awesome-tab"
  :config
  (awesome-tab-mode t))

(use-package powerline
  :defer t)

(use-package centaur-tabs
  :straight (:host github :repo "ema2159/centaur-tabs")
  :defer .1
  :init
  (setq-default centaur-tabs-set-bar 'over)
  :config
  (setq centaur-tabs-set-close-button nil
        centaur-tabs-background-color (face-background 'centaur-tabs-default)
        ;; centaur-tabs-style "wave"

        ;; centaur-tabs-style "bar"
        ;; centaur-tabs-set-bar 'over

        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-modified-marker "*"
        centaur-tabs-plain-icons t
        centaur-tabs-set-modified-marker t)

  (centaur-tabs-mode t))

(provide 'use-tabs)
