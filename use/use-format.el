;; Format source code -*- lexical-binding: t; -*-
(use-package reformatter
             :disabled t
  ;; :defer t
  )

(use-package ormolu
             :disabled t
 :hook (haskell-mode . ormolu-format-on-save-mode))
 ;; :bind
 ;; (:map haskell-mode-map
 ;;   ("C-c r" . ormolu-format-buffer)))

(provide 'use-format)
