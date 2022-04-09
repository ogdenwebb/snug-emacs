;;; Find thing or definition -*- lexical-binding: t; -*-

(use-package xref
  :commands (xref-show-definitions-completing-read)
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
   (when (and (boundp 'xref-search-program) (executable-find "rg"))
    (setq xref-search-program 'ripgrep))

  (remove-hook 'xref-backend-functions #'etags--xref-backend))

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :config
  (setq dumb-jump-default-project user-emacs-directory)
  (setq dumb-jump-selector 'completing-read))

(provide 'env-lookup)
