(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :init
  (add-hook 'go-mode-hook (lambda ()
                          (company-mode)
                          ;; (set (make-local-variable 'company-backends) '((company-go company-keywords)))
                          (add-hook 'before-save-hook #'gofmt-before-save nil t)
                          ))
  :config
  (setq gofmt-command "goimports"))
        ;; godoc-at-point-function 'godoc-gogetdoc))

(use-package company-go
  :disabled
  :after (company go-mode)
  :config
  (setq company-go-show-annotation t))

(use-package go-impl
  :after go-mode)

;; lsp provides the similar stuff
;; (use-package go-eldoc
;;   :hook (go-mode . go-eldoc-setup))

(provide 'use-go)
