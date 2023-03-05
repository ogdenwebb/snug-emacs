;; C/C++ module -*- lexical-binding: t -*-

(use-package cc-vars
  :defer t
  :elpaca nil
  :config
  (setq-default c-basic-offset snug-default-indent-width)

  (setq c-default-style '((java-mode  "java")
                          (awk-mode  "awk")
                          (other . "stroustrup"))))

(use-package modern-cpp-font-lock
  :diminish
  :hook (c++-mode . modern-c++-font-lock-mode))

(provide 'use-cc)
