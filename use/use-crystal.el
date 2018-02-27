(use-package crystal-mode
  :mode (("\\.cr\\'" . crystal-mode))
  :config
  (progn
      (add-hook 'crystal-mode-hook
                (lambda () (add-hook 'before-save-hook 'crystal-tool-format nil 'local)))))


(provide 'use-crystal)
