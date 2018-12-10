;; C/C++ module

(use-package cc-mode
  :ensure nil
  :hook (c-mode-common . (lambda ()
                           (c-set-style "stroustrup")
                           (setq c-basic-offset 4)
                           (setq tab-width 4))))

(use-package modern-cpp-font-lock
  :diminish
  :hook (c++-mode . modern-c++-font-lock-mode))

(provide 'use-cc)
