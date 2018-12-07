;; Go Packages
;; go-add-tags
;; go-stacktracer
;; go-eldoc
;; go-direx

(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :init
  (add-hook 'go-mode-hook (lambda ()
                          (company-mode)
                          (set (make-local-variable 'company-backends) '(company-go)))))

(use-package company-go
  :after (company go-mode)
  :config
  (setq company-go-show-annotation t))

(provide 'use-go)
