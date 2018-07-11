;; TODO: split to basic plugin and dev plugin
(use-package package-utils
  :ensure t
  :commands (package-utils-upgrade-all))

;; Project management
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

;; Quickrun
(use-package quickrun
  :ensure t
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region))

;; Integration with Chrome/Chromium
;; (use-package atomic-chrome
;;   :config
;;   (atomic-chrome-start-server))


;; Move region or line
(use-package drag-stuff
  :ensure t
  :commands (drag-stuff-left drag-stuff-up drag-stuff-down drag-stuff-right))
  ;; :config (drag-stuff-global-mode 1))

;; Yasnippet
;; TODO:
;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (yas-global-mode 1))

;; (use-package neotree
;;   :config
;;   (setq neo-mode-line-type 'none)
;;   :general
;;   (general-define-key :keymaps 'neotree-mode-map
;;                       :states '(normal)
;;                       "SPC" 'neotree-enter
;;                       "TAB" 'neotree-enter
;;                       "RET" 'neotree-enter
;;                       "q" 'neotree-hide))

(use-package treemacs
  :ensure t
  :commands (treemacs)
  :config
  (use-package treemacs-evil
    :after treemacs)

  ;; Disable mode-line in treemacs buffer
  ;; see: https://github.com/Alexander-Miller/treemacs/blob/d5456233909a4f558d24056a5e53f15e9f2029f6/treemacs-mode.el#L202
  (defun treemacs--setup-mode-line ()
      (setq mode-line-format nil))

  :general
  (general-define-key :keymaps 'treemacs-mode-map
                      :states  '(normal visual treemacs)
                      "M-h"  'evil-window-left
                      "M-j"  'evil-window-down
                      "M-k"  'evil-window-up
                      "M-l"  'evil-window-right))

;; (use-package which-key
;;   :init
;;   (which-key-mode)
;;   (which-key-setup-side-window-bottom))

;; Simple Emacs minor mode for a nice writing environment.
(use-package olivetti
  :ensure t
  :commands (olivetti-mode olivetti-shrink olivetti-expand olivetti-toggle-hide-mode-line)
  :config
  (setq-default olivetti-body-width 80))

(use-package google-translate
  :defer t
  :config
  (use-package google-translate-smooth-ui
    :after google-translate
    :commands google-translate-smooth-translate)
  (setq google-translate-translation-directions-alist '(("en" . "ru"))))

(use-package imenu-list
  :ensure t
  :commands (imenu-list-smart-toggle)
  :config
  (setq imenu-list-mode-line-format nil)
  (setq imenu-list-size 32))

(use-package restart-emacs
  :commands (restart-emacs))

(use-package deft
  :config
  (setq deft-extensions '("txt" "tex" "org"))
  (setq deft-directory "~/Drive/org")
  (setq deft-recursive t))


;; (use-package helpful
;;   :ensure t
;;   :general
;;   (general-define-key :states 'normal
;;                       :prefix leader
;;                       "h k" 'helpful-key
;;                       "h v" 'helpful-variable
;;                       ;; "h f" 'helpful-function
;;                       "h f" 'helpful-callable
;;                       "h l" 'find-library
;;                       "h ." 'helpful-at-point))

(provide 'env-plugins)
