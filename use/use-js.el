;; Javascript
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-idle-timer-delay 1)

  (use-package tern
    :config
    (add-hook 'js2-mode-hook (lambda () (tern-mode t)))))

(use-package indium)

(provide 'use-js)
