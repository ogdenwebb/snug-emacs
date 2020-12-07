;; Support for various filetypes -*- lexical-binding: t -*-
;; Markdown
(use-package markdown-mode
  :defer t
  ;; :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown"))

;; yaml support
(use-package yaml-mode
  :defer t
  :mode ("\\.yml\\'" . yaml-mode)
  :config

  :general
  (general-define-key :keymaps '(yaml-mode-map)
                      :states '(insert)
                      "RET" 'evil-ret-and-indent))

(use-package json-mode
  :defer t
  :mode ("\\.json\\'" . json-mode))

;; vimrc mode
(use-package vimrc-mode
  :defer t
  :mode ("/\\.?g?vimrc$"
         "\\.vim$"
         "\\.?vimperatorrc$"
         "\\.vimp$"))

;; Shell files
(use-package sh-script
  :mode (("[/\\]\\zshrc$" . shell-script-mode))
  :config
  (setq sh-learn-basic-offset t))

(use-package makefile-mode
  :straight nil
  :defer t
  :mode (("\\(/\\|\\`\\)[Mm]akefile" . makefile-mode)))

;; Performance issues
(use-package cmake-mode
  :disabled t
  :defer t
  :mode (("\\.cmake\\'" . cmake-mode)
         ("\\CMakeLists.txt$" . cmake-mode)))

(use-package vimrc-mode
  :mode ("/\\.?g?vimrc$"
         "\\.vim$"
         "\\.?vimperatorrc$"
         "\\.vimp$"))

;; Support for i3 config
(use-package i3wm-config-mode
  :straight (:host github :repo "Alexander-Miller/i3wm-Config-Mode")
  :defer t
  :mode (("[/\\]\\.config/i3/config$" . i3wm-config-mode)
         ("[/\\]\\.i3/config$" . i3wm-config-mode)))

(use-package pdf-tools
  :disabled t
  :config
  (setq-default pdf-view-display-size 'fit-width)
  (pdf-loader-install))

(use-package graphviz-dot-mode
  :defer t
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\`" . graphviz-dot-mode))
  :config
  (setq graphviz-dot-indent-width 4))

(use-package company-graphviz-dot
  :straight nil
  :after (graphviz-dot-mode company)
  )

(provide 'use-filetype)
