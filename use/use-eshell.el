;; Customize prompt
(with-eval-after-load 'eshell
  (setq eshell-prompt-function
        (lambda nil
          (concat
           (eshell/pwd)
           " ~ ❯ "))))

(provide 'use-eshell)
