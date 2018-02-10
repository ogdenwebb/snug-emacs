(use-package anaconda-mode
  :defer t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :after anaconda-mode
  :config
  (add-to-list 'company-backends 'company-anaconda))

(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset nil)

(provide 'use-py)
