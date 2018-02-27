;; On-the-fly evaluation/substitution of Emacs lisp code
(use-package elisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(use-package litable
  :mode ("\\.el\\'" . emacs-lisp-mode))

;; (use-package adjust-parens
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook #'adjust-parens-mode)
;;   (add-hook 'clojure-mode-hook #'adjust-parens-mode))

(provide 'env-lisp)
