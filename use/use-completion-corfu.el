;; Corfu - Completion Overlay Region FUnction  -*- lexical-binding: t; -*-

(use-package corfu
  :config
  ;; Optional customizations
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (setq corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  ;; (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;; (corfu-echo-documentation nil) ;; Do not show documentation in the echo area

  (setq tab-always-indent 'complete)

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
         ("C-p" . corfu-previous)
         ("C-n" . corfu-next)
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)
         ("C-;" . corfu-doc-toggle))

  ;; You may want to enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  )

;; Documentation popup for Corfu
(use-package corfu-doc
  :config
  (setq corfu-doc-delay 0.2
        corfu-doc-max-width 80
        corfu-doc-max-height 40))


(use-package kind-icon
  :after corfu
  :config
  ;; Text-based Icons
  (setq kind-icon-use-icons nil)
  (setq  kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'use-completion-corfu)
