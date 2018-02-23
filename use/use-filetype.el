;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode)
  :config
  :general
  (general-define-key :keymaps '(yaml-mode-map)
                      :states '(insert)
                      "RET" 'evil-ret-and-indent))


(use-package vimrc-mode
  :mode ("/\\.?g?vimrc$"
          "\\.vim$"
          "\\.?vimperatorrc$"
          "\\.vimp$"))

(provide 'use-filetype)
