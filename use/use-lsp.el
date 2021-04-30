;; ;; Language server protocol support -*- lexical-binding: t -*-
(use-package lsp-mode
  :straight nil
  ;; :config
  ;; (setq lsp-log-io t)
  ;; :init
  :hook (js2-mode-hook . lsp-deferred)
  :hook (php-mode . lsp-deferred)
  :hook (css-mode . lsp-deferred)
  :hook (web-mode . lsp-deferred)

  :config
  ;; (add-to-list 'lsp-language-id-configuration '(web-mode . "html-ls"))

  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "html-languageserver" "--stdio")
  ;;                   :major-modes '(web-mode)
  ;;                   :server-id 'html-languageserver))
  )
  ;; (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-ignore-duplicate t))

;; Make sure we have lsp-imenu everywhere we have LSP
(use-package lsp-ui-imenu
  :straight nil
  :hook (lsp-after-open-hook . lsp-enable-imenu))


;; ;; languages
;; (use-package lsp-html
;;   :straight nil
;;   :after lsp-mode
;;   :hook ((html-mode web-mode) . lsp-html-enable))

;; (use-package lsp-css
;;   :after lsp-mode
;;   :straight nil
;;   :hook ((css-mode-hook  . lsp-css-enable)
;;          (less-mode-hook . lsp-less-enable)
;;          (sass-mode-hook . lsp-scss-enable)
;;          (scss-mode-hook . lsp-scss-enable)))

;; (use-package company-lsp
;;   :after (company lsp-mode)
;;   :config
;;   (add-to-list 'company-backends 'company-lsp)
;;   (setq company-lsp-async t)
;;   (setq company-lsp-enable-recompletion t))

;; ;; Eglot

(use-package eglot
  :config
  (when (executable-find "lua-lsp")
      (add-to-list 'eglot-server-programs '(lua-mode . ("lua-lsp")))
      (add-hook 'lua-mode-hook #'eglot-ensure))

  (when (executable-find "nimlsp")
      (add-to-list 'eglot-server-programs '(nim-mode . ("nimlsp"))))
      (add-hook 'nim-mode-hook #'eglot-ensure))

  ;; :after (flymake json-rpc)
  ;; :hook (haskell-mode . eglot-ensure)
  ;; :config
  ;; (add-to-list 'eglot-server-programs '(haskell-mode . ("ghcide" "--lsp")))

;;   ;; HTML
;;   (cl-pushnew '(web-mode "html-languageserver" "--stdio") eglot-server-programs :key 'car)
;;   (add-hook 'web-mode-hook 'eglot-ensure)

;; ;;   ;; CSS
;;   (cl-pushnew '(css-mode "css-languageserver" "--stdio") eglot-server-programs :key 'car)
;;   (add-hook 'css-mode-hook 'eglot-ensure)
;;   )

;;   ;; (add-to-list 'eglot-ignored-server-capabilites :hoverProvider))

(provide 'use-lsp)
