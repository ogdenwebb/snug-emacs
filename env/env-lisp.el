;; On-the-fly evaluation/substitution of Emacs lisp code
(use-package litable)

;; lispy + evil = lispyville
;; (use-package lispyville
;;   :init
;;   (add-hook 'clojure-mode-hook #'lispyville-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'lispyville-mode)
;;   (add-hook 'common-lisp-mode-hook #'lispyville-mode)
;;   (add-hook 'scheme-mode-hook #'lispyville-mode)
;;   :config
;;   (lispyville-set-key-theme
;;    '(operators
;;      c-w
;;      (escape insert)
;;      (normal visual motion))))

(provide 'env-lisp)
