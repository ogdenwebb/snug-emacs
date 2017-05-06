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
  :ensure t
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  ;; (use-package flycheck-clojure
  ;;   :config
  ;;   (eval-after-load 'flycheck '(flycheck-clojure-setup)))
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :general
  (general-nvmap
   "] e" 'flycheck-next-error
   "[ e" 'flycheck-previous-error))


;;   (use-package flycheck-pos-tip
;;     :config
;;     (with-eval-after-load 'flycheck
;;       (flycheck-pos-tip-mode))))

;; Spell checking
(use-package flyspell
  :ensure t
  :config
  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  ;; (add-hook 'text-mode-hook 'flyspell-mode)
  (setq flyspell-issue-message-flag nil)
  (setq ispell-program-name (executable-find "hunspell")
        ispell-dictionary "en_US"))

;; flyspell ivy corret
(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell)

(provide 'env-check)
