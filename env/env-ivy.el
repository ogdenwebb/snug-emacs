;; Use ivy -*- lexical-binding: t -*-

(use-package ivy
  :ensure t
  :init
  (ivy-mode t)
  :config
  (setq ivy-display-style 'fancy
        ivy-height 12
        ivy-re-builders-alist
        ;; allow input not in order
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-ignore-order)))


  ;; Add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers nil)

  ;; Do not show "./" and "../" in the `counsel-find-file' completion list
  ;; (setq ivy-extra-directories nil) ; default value: ("../" "./")
  ;; (setq ivy-count-format "[%d/%d] ")
  ;; TODO: read
  ;; (setq enable-recursive-minibuffers t)

  (use-package swiper
    :commands (swiper)
    :ensure t)

  (use-package counsel
    :ensure t
    :config
    (use-package counsel-projectile
      :ensure t
      :after (counsel projectile)
      :commands (counsel-projectile-mode counsel-projectile-find-file))
    (setq projectile-completion-system 'ivy)
    (setq counsel-find-file-ignore-regexp (regexp-opt '(".jpg" ".png" ".jpeg")))
    (setq counsel-git-cmd "rg --files")
    (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
    (setq counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package ivy-rich
  :ensure t
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
