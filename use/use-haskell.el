;; Haskell -*- lexical-binding: t -*-
(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (add-hook 'haskell-mode-hook (lambda () (setq-local electric-indent-inhibit t)))

  ;; TODO: two empty lines
  (defun haskell-indentation-advice ()
    (when (and (< 1 (line-number-at-pos))
               (save-excursion
                 (forward-line -1)
                 (string= "" (s-trim (buffer-substring (line-beginning-position) (line-end-position))))))
      (delete-region (line-beginning-position) (point))))

  (advice-add 'haskell-indentation-newline-and-indent
              :after 'haskell-indentation-advice))

(use-package flycheck-haskell
  :defer t
  :hook (haskell-mode . flycheck-haskell-setup))

(use-package dante
  :after haskell-mode
  :disabled t
  :commands (dante-mode)
  :hook (haskell-mode . dante-mode))

(provide 'use-haskell)
