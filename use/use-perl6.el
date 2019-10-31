(use-package perl6-detect
  :straight (:host github :repo "perl6/perl6-mode"))

(use-package flycheck-perl6
  :after perl6-mode
  :config (add-hook 'perl6-mode-hook #'flycheck-mode))

(provide 'use-perl6)
