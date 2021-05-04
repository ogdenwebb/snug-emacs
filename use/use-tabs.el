;;; Tabs -*- lexical-binding: t -*-

;; (use-package powerline
;;   :defer t)

(use-package centaur-tabs
  ;; :hook ((imenu-list-major-mode . centaur-tabs-local-mode))
  :config
  (centaur-tabs-mode t)
  ;; (centaur-tabs-headline-match)


  (setq centaur-tabs-set-close-button nil
        ;; centaur-tabs-background-color (face-background 'centaur-tabs-default)
        ;; centaur-tabs-style "wave"

        ;; centaur-tabs-style "bar"
        ;; centaur-tabs-set-bar 'over

        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-modified-marker "*"
        centaur-tabs-plain-icons t
        centaur-tabs-set-modified-marker t))

;; Create `after-load-theme-hook'.
;; TODO submit this upstream?
;; (defvar after-load-theme-hook nil
;;   "Hook run after a color theme is loaded using `load-theme'.")

;; (defadvice load-theme (after run-after-load-theme-hook activate)
;;   "Run `after-load-theme-hook'."
;;   (run-hooks 'after-load-theme-hook))

;; (defun reset-tabs-background ()
;;   "test"
;;   (interactive)
;;   (setq centaur-tabs-background-color (face-background 'default)))

;; (add-hook 'after-load-theme-hook #'reset-tabs-background)
;; (add-hook 'after-load-theme-hook #'centaur-tabs-display-update)

(provide 'use-tabs)
