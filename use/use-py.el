(use-package anaconda-mode
  :after python
  :hook python-mode
  :config
  (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :after anaconda-mode
  :config
  (add-to-list 'company-backends 'company-anaconda))

(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset nil)

(provide 'use-py)
