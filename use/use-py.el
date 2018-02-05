(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :after company
  :config
  (add-to-list 'company-backends 'company-anaconda))

(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset nil)

(provide 'use-py)
