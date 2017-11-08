;; Javascript
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-idle-timer-delay 1)
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2)
  (setq js2-bounce-indent-p t)

  (use-package tern
    :config
    (add-hook 'js2-mode-hook (lambda () (tern-mode t)))))

(use-package indium
  :init
  (add-hook 'js-mode-hook #'indium-interaction-mode))

(provide 'use-js)
