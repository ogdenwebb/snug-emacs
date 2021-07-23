;;; Syntax & spell checking -*- lexical-binding: t; -*-

;; Package-lint
(use-package package-lint
  :disabled t
  :commands (package-lint-current-buffer))

;; Flycheck
(use-package flycheck
  :commands (flycheck-mode global-flycheck-mode flycheck-list-errors flycheck-buffer)
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-idle-change-delay (if flycheck-current-errors 0.3 3.0)
        flycheck-emacs-lisp-load-path 'inherit
        ;; MAYBE:
        ;; flycheck-standard-error-navigation nil
        )

  ;; TODO: enable
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc flycheck-haskell haskell-stack-ghc haskell-ghc))

  ;; TODO: ????
  ;; (make-variable-buffer-local 'flycheck-idle-change-delay)
  ;; Disable fringe markers
  ;; (setq flycheck-indication-mode nil)
  (setq flycheck-indication-mode 'right-fringe)

  ;; Customize fringe bitmap

  ;; TODO: indicators in the corner of the sceen without padding
  ;; after window resize
  ;; (when (fboundp 'define-fringe-bitmap)
  ;;   (define-fringe-bitmap 'snug/flycheck-fringe-indicator
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
    (define-fringe-bitmap 'snug/flycheck-fringe-error
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
    (define-fringe-bitmap 'snug/flycheck-fringe-indicator
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
    :fringe-bitmap 'snug/flycheck-fringe-error
    :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'snug/flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)

  (flycheck-define-error-level 'info
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'snug/flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info))

;; Emacs mode to fix the flycheck error at point
(use-package attrap
  :after flycheck)

(use-package flycheck-package
  :disabled t
  :after flycheck
  :hook (elisp-mode . flycheck-package-setup))

;; TODO:
;; (use-package flycheck-pos-tip
;;   :after flycheck
;;   :config
;;   (flycheck-pos-tip-mode))

;; Display Flycheck errors in tooltip
(use-package flycheck-posframe
  ;; :if (featurep 'posframe)
  :hook ((global-flycheck-mode flycheck-mode) . flycheck-posframe-mode)
  :config
  ;; Default Pretty Configuration
  (flycheck-posframe-configure-pretty-defaults))

;; (use-package flycheck-pos-tip
;;   :hook ((global-flycheck-mode flycheck-mode) . flycheck-pos-tip-mode)
;;   :config (setq flycheck-pos-tip-timeout 30))

;; TODO:
;; (use-package flycheck-popup-tip
;;   :if (not (display-graphic-p))
;;   :hook ((global-flycheck-mode flycheck-mode) . flycheck-popup-tip-mode))

;; Spell checking
(use-package flyspell
  :disabled t
  ;; :commands (flyspell-mode flyspell-buffer)
  :config
  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  ;; (add-hook 'text-mode-hook 'flyspell-mode)
  ;; Reduce flyspell noise
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)


  ;; NO spell check for embedded snippets
  (defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)

    (let* ((rlt ad-return-value)
           (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\|example\\|quote\\)")
           (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\|example\\|quote\\)")
           (case-fold-search t)
           b e)
      (when ad-return-value
        (save-excursion
          (setq b (re-search-backward begin-regexp nil t))
          (if b (setq e (re-search-forward end-regexp nil t))))
        (if (and b e (< (point) e)) (setq rlt nil)))
      (setq ad-return-value rlt)))

  )

(use-package flyspell-lazy
  :disabled t
  :commands (flyspell-lazy-mode flyspell-lazy-check-buffer)
  :hook (flyspell-lazy-mode . flyspell-mode))

(with-eval-after-load 'flyspell
  (defun snug/flyspell-set-dict (dict)
    (progn
      (if (not (bound-and-true-p flyspell-mode))
          (flyspell-mode))
      (ispell-change-dictionary dict))))
      ;; (flyspell-buffer))))

;; (with-eval-after-load 'spell-fu
;;   (defun snug/flyspell-set-dict (dict)
;;     (ispell-change-dictionary dict)
;;     (if (not (bound-and-true-p spell-fu-mode))
;;         (spell-fu-mode))
;;     ))

;; (with-eval-after-load 'wucuo
;;   (defun snug/flyspell-set-dict (dict)
;;     (ispell-change-dictionary dict)
;;     ;; (wucuo-start)
;;     ;; (if (not (bound-and-true-p wucuo-mode))
;;     ;;     (wucuo-start))
;;     ))

;; flyspell ivy correct
(use-package flyspell-correct
  :after flyspell
  :commands (flyspell-correct-at-point flyspell-correct-next-word-generic
                                       flyspell-correct-previous flyspell-correct-previous-word-generic
                                       flyspell-correct-word flyspell-correct-word-generic))

(use-package flyspell-correct-ivy
  :after (ivy flyspell-correct))

(use-package flyspell-correct-helm
  :after (helm flyspell-correct))

(use-package ispell
  :defer 2
  :config
  ;; (setq ispell-program-name (or (executable-find "hunspell")
  ;;                               (executable-find "aspell"))

  (setq ispell-program-name "aspell"
        ispell-dictionary "en_US"
        ispell-quietly t
        ispell-silently-savep t
        ;; ispell-really-hunspell t
        )

  ;; Skip some parts in org-mode
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))

  ;; If you are using aspell instead of ispell on the backend,
  ;; the following setting may improve performance
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")


  (defun snug/ispell-set-dict (dict)
    "Set dictionary for ispell."
    (ispell-change-dictionary dict))


  (defhydra hydra-ispell (:color blue)
    "Ispell dictionary"
    ;; for aspell
    ("e" (snug/ispell-set-dict "en") "eng")
    ("r" (snug/ispell-set-dict "ru") "rus")
    ("g" (snug/ispell-set-dict "german") "ger")
    ("q" nil "cancel"))
  )
;; (use-package spell-fu
;;   :hook (org-mode . (lambda()
;;                       (setq spell-fu-faces-exclude
;;                             '(org-meta-line org-link org-code))
;;                       ;; (spell-fu-mode)
;;                       )))


;; (use-package wucuo
;;   :config
;;   (setq wucuo-flyspell-start-mode 'fast))

;; Spell check in Emacs using Flycheck/Flymake and Aspell
(use-package flycheck-aspell
  :disabled t
  :config
  (add-to-list 'flycheck-checkers 'tex-aspell-dynamic)

  (advice-add #'ispell-pdict-save :after #'flycheck-maybe-recheck)
  (defun flycheck-maybe-recheck (_)
    (when (bound-and-true-p flycheck-mode)
      (flycheck-buffer))))

(use-package flymake
  :straight nil
  :defer t
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package flymake-quickdef
  :commands (flymake-quickdef-backend))

(use-package flymake-aspell
  :commands (flymake-aspell-setup))

(provide 'env-check)
