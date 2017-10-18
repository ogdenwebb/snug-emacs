(use-package hydra
  :config
  (defhydra hydra-flyspell (:color teal)
    "Flyspell"
    ("r" (lambda ()
           (interactive)
           (ispell-change-dictionary "ru_RU")
           (flyspell-buffer))
     "rus")
    ("e" (lambda ()
           (interactive)
           (ispell-change-dictionary "en_US")
           (flyspell-buffer))
     "en_US")
    ("d" (lambda ()
           (interactive)
           (ispell-change-dictionary "de_DE")
           (flyspell-buffer))
     "de")
    ("q" nil "cancel")))

;; Package-lint
(use-package package-lint
  :after flycheck)

;; Flycheck
(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; (use-package flycheck-clojure
  ;;   :config
  ;;   (eval-after-load 'flycheck '(flycheck-clojure-setup)))
  :config
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.3 3.0))

  ;; TODO: enable
  ;; (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))

  (make-variable-buffer-local 'flycheck-idle-change-delay)
  ;; Disable fringe markers
  ;; (setq flycheck-indication-mode nil)
  (setq flycheck-indication-mode 'right-fringe)

  ;; Customize fringe bitmap

  (when (fboundp 'define-fringe-bitmap))
  (define-fringe-bitmap 'elmax/flycheck-fringe-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b01110000
            #b11111000
            #b11111000
            #b11111000
            #b01110000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))

  (flycheck-define-error-level 'error
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'elmax/flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'elmax/flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)

  (flycheck-define-error-level 'info
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'elmax/flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info))

;;   (use-package flycheck-pos-tip
;;     :config
;;     (with-eval-after-load 'flycheck
;;       (flycheck-pos-tip-mode))))

;; Spell checking
(use-package flyspell
  :config
  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  ;; (add-hook 'text-mode-hook 'flyspell-mode)
  (setq flyspell-issue-message-flag nil)
  (setq ispell-program-name (executable-find "hunspell")
        ispell-dictionary "en_US"))

;; flyspell ivy corret
(use-package flyspell-correct-ivy
  :after flyspell)

(provide 'env-check)
