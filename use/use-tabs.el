(use-package awesome-tab
  :disabled t
  :load-path "dev/awesome-tab"
  :config
  (awesome-tab-mode t)
)

(use-package centaur-tabs
  :disabled t
  :straight (:host github :repo "ema2159/centaur-tabs")
   :config
   (setq centaur-tabs-background-color (face-background 'centaur-tabs-default))
   (setq centaur-tabs-set-close-button nil)
   (setq centaur-tabs-style "bar"
         centaur-tabs-height 32
         centaur-tabs-set-icons t)
   (centaur-tabs-mode t)
   )

(provide 'use-tabs)
