(use-package crystal-mode
  :mode (("\\.cr\\'" . crystal-mode))
  :config
  (add-hook 'crystal-mode-hook
            (lambda () (add-hook 'before-save-hook 'crystal-tool-format nil 'local))))

(use-package flycheck-crystal
  :after (crystal-mode flycheck))

(provide 'use-crystal)
