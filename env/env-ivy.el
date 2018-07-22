;; Use ivy for fuzzy matching -*- lexical-binding: t -*-

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  :config

  ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

  ;; Add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers nil)

  ;; Do not show "./" and "../" in the `counsel-find-file' completion list
  (setq ivy-extra-directories nil) ; default value: ("../" "./")
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
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
  (setq ivy-rich-path-style 'abbrev))

(provide 'env-ivy)
