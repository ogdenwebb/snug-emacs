;; (use-package lispyville
;;   :init
;;   (add-hook 'clojure-mode-hook #'lispyville-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'lispyville-mode)
;;   (add-hook 'common-lisp-mode-hook #'lispyville-mode)
;;   (add-hook 'scheme-mode-hook #'lispyville-mode)
;;   :general
;;   (general-define-key :keymaps '(emacs-lisp-mode-map clojure-mode-map common-lisp-mode-map)
;;                       :states  '(insert)
;;                       "(" 'lispy-parens))

; Let's simplify the way we write Lisp
(use-package parinfer
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             ;; pretty-parens  ; different paren styles for different modes.
             evil           ; If you use Evil.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)))

;; On-the-fly evaluation/substitution of Emacs lisp code
(use-package litable)

(provide 'env-lisp)
