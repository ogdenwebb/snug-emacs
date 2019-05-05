;; C/C++ module -*- lexical-binding: t -*-
(use-package cc-mode
  :ensure nil
  :hook (c-mode-common . (lambda ()
                           (c-set-style "stroustrup")
                           (setq-default c-basic-offset 4
                                         c-basic-indent 4)
                           (setq c-basic-offset 4
                                 tab-width 4))))

(use-package modern-cpp-font-lock
  :diminish
  :hook (c++-mode . modern-c++-font-lock-mode))

(provide 'use-cc)
