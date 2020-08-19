;; Helm is incremental completion and selection narrowing framework -*- lexical-binding: t -*-

(defvar snug-with-helm-fuzzy t
  "Enable fuzzy matching for Helm, t by default.")

(use-package helm
  :defer t
  :hook ((after-init . helm-mode)
         (helm-mode . helm-autoresize-mode))
  :config
  ;; No idea why here find-file is set to nil (so it uses the native find-file
  ;; for Emacs. This makes stuff like (find-file (read-file-name ...)) work with
  ;; Helm again.
  ;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . helm-completing-read-symbols))

  (use-package helm-config
               :straight nil)

  ;; Fuzzy stuff
  (when snug-with-helm-fuzzy
    (setq helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-locate-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-apropos-fuzzy-match t))

  ;; Customize Helm
  (setq helm-split-window-inside-p t
        helm-ff-search-library-in-sexp t        ; search for library in `require' and `declare-function' sexp.
        helm-ff-file-name-history-use-recentf t ; Use `recentf-list' instead of `file-name-history'.
        helm-display-header-line nil
        helm-display-buffer-height 30
        helm-autoresize-max-height 30
        helm-autoresize-min-height 30)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  ;; Disable helm mode-line
  ;; (setq-default helm-mode-line-string nil)
  ;; (setq mode-line-format (default-value 'mode-line-format))

  ;; Set helm as default completion system
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'helm)))

(use-package helm-projectile
  :defer t
  :after (helm projectile)
  :hook (projectile-mode-hook . helm-projectile-on))

;; Helm Descbinds provides an interface to emacs’ describe-bindings making the currently active key bindings interactively searchable with helm.
(use-package helm-descbinds
  :defer t
  :after helm
  :hook (helm-mode . helm-descbinds-mode))

(use-package helm-swoop
  :defer t
  :after helm)

;; Ag support for helm
(use-package helm-ag
  :defer t
  :after helm
  :config
  (setq helm-ag-fuzzy-match          t
        helm-ag-use-grep-ignore-list t
        helm-ag-use-agignore         t))

(use-package helm-ls-git
  :defer t
  :after helm)

;; (use-package swiper-helm
;;   :after helm)

(use-package helm-org
  :after (helm org))

;; Rifle through your Org-mode buffers and acquire your target
(use-package helm-org-rifle
  :defer t
  :after helm)

(use-package helm-company
  :defer t
  :after (helm company))

(use-package helm-eshell
  :defer t
  :commands (helm-eshell-history))

(use-package helm-css-scss
  :defer t
  :commands (helm-css-scss helm-css-scss-multi))

;; helm fzf
(defun my-helm-run-fzf (candidate)
  (interactive)
  (let ((helm-current-dir (file-name-directory (helm-get-selection))))
      (fzf/start helm-current-dir)))

(defun my-helm-ff-switch-to-fzf ()
  "Stop helm find-files and use fzf"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'my-helm-run-fzf)))

(use-package helm-files
  :bind (("C-x C-f" . helm-find-files)
         :map helm-find-files-map
         ("C-," . my-helm-ff-switch-to-fzf)
         ("<C-backspace>" . helm-find-files-up-one-level))
  :config
  (unless helm-source-find-files
    (setq helm-source-find-files (helm-make-source
                                  "Find Files" 'helm-source-ffiles)))
  (helm-add-action-to-source "C-, Switch to fzf" #'my-helm-run-fzf helm-source-find-files))

(provide 'env-helm)
