(use-package elm-mode
  :config
  (withe-eval-after-load 'company
    (add-to-list 'company-backends 'company-elm)))

(provide 'use-elm)
