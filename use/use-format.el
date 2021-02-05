;; Format source code -*- lexical-binding: t; -*-
(use-package reformatter
  :defer t)

(use-package ormolu
 :hook (haskell-mode . ormolu-format-on-save-mode))
 ;; :bind
 ;; (:map haskell-mode-map
 ;;   ("C-c r" . ormolu-format-buffer)))

(provide 'use-format)
