;;; Syntax & spell checking -*- lexical-binding: t; -*-

(use-package hydra
  :defer t
  :config
  (defhydra hydra-flyspell (:color teal)
    "Flyspell dictionary"
    ("r" (elmax/flyspell-set-dict "ru") "rus")
    ("e" (elmax/flyspell-set-dict "en") "eng")
    ("d" (elmax/flyspell-set-dict "de") "den")
    ("q" nil "cancel")))

;; Package-lint
(use-package package-lint
  :commands (package-lint-current-buffer))

;; Flycheck
(use-package flycheck
  :ensure t
  :commands (flycheck-mode global-flycheck-mode flycheck-list-errors flycheck-buffer)
  :config
  (add-hook 'prog-mode-hook #'flycheck-mode)

  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.3 3.0))

  ;; TODO: enable
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))

  ;; TODO: ????
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  ;; Disable fringe markers
  ;; (setq flycheck-indication-mode nil)
  (setq flycheck-indication-mode 'right-fringe)

  ;; Customize fringe bitmap

  ;; TODO: indicators in the corner of the sceen without padding
  ;; after window resize
  ;; (when (fboundp 'define-fringe-bitmap)
  ;;   (define-fringe-bitmap 'elmax/flycheck-fringe-indicator
  ;;     (vector #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00111000
  ;;             #b01111100
  ;;             #b01111100
  ;;             #b01111100
  ;;             #b00111000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000)))

  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'elmax/flycheck-fringe-error
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b11000011
              #b11100111
              #b01100110
              #b00111100
              #b00111100
              #b01100110
              #b11100111
              #b11000011
              #b00000000
              #b00000000
              #b00000000
              #b00000000)))

  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'elmax/flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000001
              #b00000111
              #b00001111
              #b00011111
              #b00111111
              #b00111111
              #b00011111
              #b00001111
              #b00000111
              #b00000001
              #b00000000
              #b00000000
              #b00000000)))


  (flycheck-define-error-level 'error
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'elmax/flycheck-fringe-error
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
  :commands (flyspell-mode flyspell-buffer)
  :config
  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  ;; (add-hook 'text-mode-hook 'flyspell-mode)
  (setq flyspell-issue-message-flag nil)
  ;; aspell, hunspell
  (setq ispell-program-name (executable-find "aspell")
        ispell-dictionary "en_US"))

(defun elmax/flyspell-set-dict (dict)
    (progn
      (if (not (bound-and-true-p flyspell-mode))
        (flyspell-mode))
      (ispell-change-dictionary dict)
      (flyspell-buffer)))

;; flyspell ivy corret
(use-package flyspell-correct
  :after flyspell
  :config
  (use-package flyspell-correct-ivy))

(provide 'env-check)
