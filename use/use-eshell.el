;; Customize prompt
(setq eshell-prompt-function
  (lambda nil
    (concat
     (eshell/pwd)
     " ~ ❯ ")))

(provide 'use-eshell)
