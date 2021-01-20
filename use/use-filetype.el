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
  :mode (("[/\\]\\zshrc$" . shell-script-mode))
  :config
  (setq sh-learn-basic-offset t))

(use-package makefile-mode
  :straight nil
  :mode (("\\(/\\|\\`\\)[Mm]akefile" . makefile-mode)))

;; Performance issues
(use-package cmake-mode
  :disabled t
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
  :mode (("[/\\]\\.config/i3/config$" . i3wm-config-mode)
         ("[/\\]\\.i3/config$" . i3wm-config-mode)))

(use-package pdf-tools
  :disabled t
  :config
  (setq-default pdf-view-display-size 'fit-width)
  (pdf-loader-install))

(use-package graphviz-dot-mode
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\`" . graphviz-dot-mode))
  :config
  (setq graphviz-dot-indent-width 4))

(use-package company-graphviz-dot
  :straight nil
  :after (graphviz-dot-mode company)
  )

(provide 'use-filetype)
