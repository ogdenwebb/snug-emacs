;; maybe: https://github.com/dougm/go-projectile/
(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :interpreter "go"
  :init
  (add-hook 'go-mode-hook (lambda ()
                          (setq flycheck-disabled-checkers '(go-vet))
                          (company-mode)
                          ;; (setq company-backends 'company-go)
                          ;; (set (make-local-variable 'company-backends) '((company-go company-keywords)))
                          (add-hook 'before-save-hook #'gofmt-before-save nil t)
                          ))
  :config
  (setq gofmt-command "goimports")
        ;; godoc-at-point-function 'godoc-gogetdoc)
  (if (not (executable-find "goimports"))
      (warn "go-mode: couldn't find goimports; no code formatting/fixed imports on save")))

(use-package company-go
  :after (company go-mode)
  :config
  (setq company-go-show-annotation t))

(use-package go-impl
  :disabled t
  :after go-mode)

;; (use-package gotest
;;   :disabled t
;;   :after go-mode)

;; (use-package go-guru
;;   :after go-mode)

(use-package go-eldoc
  :if (and (not (featurep 'lsp-mode))
           (not (featurep 'eglot)))
  :hook (go-mode . go-eldoc-setup))

(provide 'use-go)
