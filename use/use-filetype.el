;; Support for various filetypes -*- lexical-binding: t -*-
;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
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
  :mode (("[/\\]\\zshrc$" . shell-script-mode)))

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


(provide 'use-filetype)
