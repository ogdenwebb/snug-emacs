(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode))

;; (defun haskell-setup ()
;;   "Setup variables for editing Haskell files."
;;   (make-local-variable 'tab-stop-list)
;;   (setq tab-stop-list (number-sequence 0 120 4))
;;   ;; (setq indent-line-function 'tab-to-tab-stop)
;;   (setq indent-line-function 'indent-relative))
;;   ;; (haskell-indentation-mode 0))
;; (add-hook 'haskell-mode-hook 'haskell-setup)

;; (use-package shm
;;   :after haskell-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'structured-haskell-mode))

;; (defun haskell-setup ()
;;   "Setup variables for editing Haskell files."
;;   (haskell-indentation-mode 0)
;;   (haskell-indent-mode 1))

(add-hook 'haskell-mode-hook 'haskell-setup)
(add-hook 'haskell-cabal-mode-hook 'haskell-setup)

(add-hook 'haskell-mode-hook (lambda () (setq electric-indent-inhibit t)))
;; (add-hook 'haskell-mode-hook (lambda () (electric-indent-local-mode -1))


(provide 'use-haskell)
