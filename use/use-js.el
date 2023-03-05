;; Javascript
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-idle-timer-delay 1)
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2)
  (setq js2-bounce-indent-p t))

(use-package tern
  :disabled t
  :after js2-mode
  :config
  (add-hook 'js2-mode-hook #'tern-mode))

;; Tern
(use-package company-tern
  :elpaca nil
  :after (company tern)
  :commands (company-tern)
  :config
  (add-to-list 'company-backends 'company-tern))


(use-package indium
  :disabled t
  :commands (indium-interaction-mode)
  :config
  (add-hook 'js-mode-hook #'indium-interaction-mode))

;; TODO: fir fox elpaca
(use-package npm
  :disabled t
  :commands (npm))

;; TODO: fir fox elpaca
(use-package npm-mode
  :disabled t
  :defer t)


(provide 'use-js)
