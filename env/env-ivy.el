;; Use ivy -*- lexical-binding: t -*-

(use-package ivy
  ;; :hook (after-init . ivy-mode)
  :init
  (ivy-mode t)
  :config
  (setq ivy-display-style 'fancy
        ivy-height 12
        ;; allow input not in order
        ;; ivy-re-builders-alist
        ;; '((read-file-name-internal . ivy--regex-fuzzy)
        ;;   (t . ivy--regex-ignore-order))

        ;; Disable initial regexp
        ivy-initial-inputs-alist nil
        ;; Add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
        ivy-use-virtual-buffers t
        ;; Do not show "./" and "../" in the `counsel-find-file' completion list
        ;; ivy-extra-directories nil ; default value: ("../" "./")`'
        ivy-count-format "[%d/%d] ")

  ;; Integrate ivy in projectile and magit
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy))
  )

(use-package ivy-hydra
  :requires (hydra)
  :after (ivy)
  :defer t)

(use-package swiper
  :commands (swiper))

(use-package counsel
  :hook (after-init . counsel-mode)
  :config
  (setq counsel-find-file-ignore-regexp (regexp-opt '(".jpg" ".png" ".jpeg")))

  ;; (when (execute-))
        ;; counsel-git-cmd "rg --files"
        ;; counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
        ;; counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode t))

;; More friendly interface for ivy
(use-package ivy-rich
  ;; :disabled t
  :after ivy
  :config
  ;; (dolist (cmd
  ;;          '(ivy-switch-buffer
  ;;            ivy-switch-buffer-other-window
  ;;            counsel-projectile-switch-to-buffer))
  ;;   (ivy-set-display-transformer cmd #'ivy-rich-switch-buffer-transformer))
  (setq ivy-rich-display-transformers-list (plist-put
            ivy-rich-display-transformers-list 'counsel-helpful-keymap-describe
            '(:columns ((counsel-describe-variable-transformer (:width 40))
                  (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))))
  (ivy-rich-set-display-transformer)
  (setq ivy-rich-path-style 'abbrev)
  (ivy-rich-mode t))

(use-package ivy-xref
  :after ivy
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(provide 'env-ivy)
