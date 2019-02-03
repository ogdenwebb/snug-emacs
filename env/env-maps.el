;; Reverse mapping for keyboard layouts other than english
(use-package reverse-im
  :config
  ;; (add-to-list 'reverse-im-modifiers 'super)
  (add-to-list 'reverse-im-input-methods "russian-computer")
  (reverse-im-mode t))

(use-package general
  :config
  (general-evil-setup)
  (setq leader "SPC")) ; set leader key

(provide 'env-maps)
