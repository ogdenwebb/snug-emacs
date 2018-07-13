;; On-the-fly evaluation/substitution of Emacs lisp code
(use-package elisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(use-package litable
  :disabled t)

(provide 'env-lisp)
