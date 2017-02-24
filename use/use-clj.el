(use-package clojure-mode
  :config
  ;; Boot
  (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
  (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode)))

(use-package cider)

(provide 'use-clj)
