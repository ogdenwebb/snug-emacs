;; Helm is incremental completion and selection narrowing framework -*- lexical-binding: t -*-

(use-package helm
  :defer t
  :hook (after-init . helm-mode)
  :config
  ;; No idea why here find-file is set to nil (so it uses the native find-file
  ;; for Emacs. This makes stuff like (find-file (read-file-name ...)) work with
  ;; Helm again.
  ;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . helm-completing-read-symbols))
  ;; Configure helm
  ;; (setq helm-display-buffer-height 12)

  ;; Fuzzy stuff
  ;; (setq helm-M-x-fuzzy-match t
  ;;       helm-buffers-fuzzy-matching t
  ;;       helm-recentf-fuzzy-match t
  ;;       helm-lisp-fuzzy-completion t)

  ;; Set helm as default completion system
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'helm))
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'helm))
)

(use-package helm-projectile
  :defer t
  :after (helm projectile)
  :hook (projectile-mode-hook . helm-projectile-on))

;; Helm Descbinds provides an interface to emacs’ describe-bindings making the currently active key bindings interactively searchable with helm.
(use-package helm-descbinds
  :defer t
  :after helm
  :hook (helm-mode . helm-descbinds-mode))

;; Rifle through your Org-mode buffers and acquire your target
(use-package helm-org-rifle
  :defer t
  :after helm)

(use-package helm-swoop
  :defer t
  :after helm)

;; Ag support for helm
(use-package helm-ag
  :defer t
  :after helm)

(use-package helm-ls-git)

(provide 'env-helm)
