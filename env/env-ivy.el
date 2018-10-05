;; Use ivy -*- lexical-binding: t -*-

(use-package ivy
  :init
  (ivy-mode t)
  :config
  (setq ivy-display-style 'fancy
        ivy-height 12
        ;; allow input not in order
        ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-ignore-order))
        )

  (setq ivy-use-virtual-buffers nil ;; Add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
        ;; Do not show "./" and "../" in the `counsel-find-file' completion list
        ;; ivy-extra-directories nil ; default value: ("../" "./")`'
        ivy-count-format "[%d/%d] ")
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))
  ;; TODO: read
  ;; (setq enable-recursive-minibuffers t)
  )

(use-package swiper
  :commands (swiper))

(use-package counsel
  :config
  (setq counsel-find-file-ignore-regexp (regexp-opt '(".jpg" ".png" ".jpeg"))
        counsel-git-cmd "rg --files"
        counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
        counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package counsel-projectile
  :after (counsel projectile)
  :commands (counsel-projectile-mode counsel-projectile-find-file))

(use-package ivy-rich
  :after ivy
  :config
  ;; (dolist (cmd
  ;;          '(ivy-switch-buffer
  ;;            ivy-switch-buffer-other-window
  ;;            counsel-projectile-switch-to-buffer))
  ;;   (ivy-set-display-transformer cmd #'ivy-rich-switch-buffer-transformer))
  (setq ivy-rich-path-style 'abbrev)
  (ivy-rich-mode t))

(provide 'env-ivy)
