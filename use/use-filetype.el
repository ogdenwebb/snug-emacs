;; Support for various filetypes -*- lexical-binding: t -*-
;; Markdown
(use-package markdown-mode
  ;; :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown"))

;; yaml support
(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode)
  :config

  :general
  (general-define-key :keymaps '(yaml-mode-map)
                      :states '(insert)
                      "RET" 'evil-ret-and-indent))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

;; vimrc mode
(use-package vimrc-mode
  :mode ("/\\.?g?vimrc$"
         "\\.vim$"
         "\\.?vimperatorrc$"
         "\\.vimp$"))

;; Shell files
(use-package sh-script
  :ensure nil
  :mode (("[/\\]\\zshrc$" . shell-script-mode))
  :config
  (setq sh-learn-basic-offset t))

(use-package makefile-mode
  :ensure nil
  :mode (("\\(/\\|\\`\\)[Mm]akefile" . makefile-mode)))

;; Performance issues
(use-package cmake-mode
  :disabled t
  :mode (("\\.cmake\\'" . cmake-mode)
         ("\\CMakeLists.txt$" . cmake-mode)))

;; Support for i3 config
(use-package i3wm-config-mode
  :ensure (:repo "https://github.com/Alexander-Miller/i3wm-Config-Mode")
  :mode (("[/\\]\\.config/i3/config$" . i3wm-config-mode)
         ("[/\\]\\.i3/config$" . i3wm-config-mode)))

(use-package pdf-tools
  :disabled t
  :config
  (setq-default pdf-view-display-size 'fit-width)
  (pdf-loader-install))

;; Graphviz support
(use-package graphviz-dot-mode
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\`" . graphviz-dot-mode))
  :config
  (setq graphviz-dot-indent-width 4))

(use-package company-graphviz-dot
  :ensure nil
  :after (graphviz-dot-mode company)
  )

;; An Emacs major mode for editing Nix expressions.
(use-package nix-mode
  ;; :disabled t
  :mode "\\.nix\\'")

(use-package fb2-reader
  :mode ("\\.fb2\\(\\.zip\\)?\\'" . fb2-reader-mode)
  :commands (fb2-reader-continue)
  :config
  (custom-theme-set-faces
   'user
   '(fb2-reader-default     ((t (:inherit variable-pitch :height 1.2))))
   '(fb2-reader-title       ((t (:inherit fb2-reader-default :height 1.4 :weight bold))))
   )

  :custom
  ;; This mode renders book with fixed width, adjust to your preferences.
  (fb2-reader-page-width 80)
  (fb2-reader-image-max-width 400)
  (fb2-reader-image-max-height 400))

(provide 'use-filetype)
