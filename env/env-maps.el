;; Reverse mapping for keyboard layouts other than english
(use-package reverse-im
  :config
  ;; (add-to-list 'reverse-im-modifiers 'super)
  (add-to-list 'reverse-im-input-methods "russian-computer")
  (reverse-im-mode t))

(use-package general
  :config
  (general-evil-setup))

;; Define leader key
(defvar snug-leader "SPC"
  "Initial <leader> key for Evil mode.")

(provide 'env-maps)
