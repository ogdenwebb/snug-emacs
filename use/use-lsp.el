;; (use-package lsp-mode
;;   :disabled t
;;   :config
;;   ;; (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)

;;   ;; Make sure we have lsp-imenu everywhere we have LSP
;;   (use-package lsp-imenu
;;     :config
;;     (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

;;   (use-package lsp-ui
;;     :config
;;     (setq lsp-ui-sideline-ignore-duplicate t)
;;     (add-hook 'lsp-mode-hook 'lsp-ui-mode)))


;; ;; languages
;; (use-package lsp-html
;;   :disabled t
;;   :hook ((html-mode web-mode) . lsp-html-enable))

;; (use-package lsp-css
;;   :disabled t
;;   :hook ((css-mode-hook  . lsp-css-enable)
;;          (less-mode-hook . lsp-less-enable)
;;          (sass-mode-hook . lsp-scss-enable
;;          (scss-mode-hook . lsp-scss-enable)))

;; (use-package company-lsp
;;   :after (company lsp-mode)
;;   :config
;;   (add-to-list 'company-backends 'company-lsp)
;;   (setq company-lsp-async t)
;;   (setq company-lsp-enable-recompletion t))

(use-package flymake
  :disabled t
  :defer t)

(use-package json-rpc
  :disabled t
  :defer t)

(use-package eglot
  :disabled t
  ;; :after (flymake json-rpc)
  :hook (go-mode . eglot-ensure)
  :config)
  ;; (add-to-list 'eglot-ignored-server-capabilites :hoverProvider))

(provide 'use-lsp)
