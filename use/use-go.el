;; Support for golang -*- lexical-binding: t -*-
;; maybe: https://github.com/dougm/go-projectile/
(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :interpreter "go"
  :config
  (defun snug/setup-go ()
    ;; (set (make-local-variable 'flycheck-disabled-checkers) '(go-vet))
    ;; (flycheck-mode -1)
    ;; (company-mode)
    ;; (set (make-local-variable 'company-backends) '(company-go))
    (set (make-local-variable 'compile-command) (concat "go run " (shell-quote-argument buffer-file-name)))
    (add-hook 'before-save-hook #'gofmt-before-save nil t)
    )

  (setq gofmt-command "goimports")
        ;; godoc-at-point-function 'godoc-gogetdoc)
  (if (not (executable-find "goimports"))
      (warn "go-mode: couldn't find goimports; no code formatting/fixed imports on save"))
  :hook (go-mode . snug/setup-go))

;; ;; Customize compile command to run go build
;; (if (not (string-match "go" compile-command))
;;     (set (make-local-variable 'compile-command)
;;          "go build -v && go test -v && go vet"))

(use-package company-go
  :requires (company)
  :after (company go-mode)
  :config
  (setq company-go-show-annotation t))

(use-package go-impl
  :after go-mode
  :config
  ;; (general-add-advice '(go-impl go-impl--completing-function)
  ;;                     :around #'without-orderless)
  ;; :config
  ;; (advice-add 'go-impl :around #'without-orderless)
  ;; (advice-add 'go-impl--receiver-complete :around #'without-orderless)
  ;; (advice-add 'go-impl--matched-packages :around #'without-orderless)
  ;; EXPLORE:
  ;; (advice-add 'go-impl--completing-function :around #'without-orderless)
  )

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
