;; Tree sitter settings  -*- lexical-binding: t -*-

(use-package treesit
  :elpaca nil
  :init
  (when (boundp 'treesit-extra-load-path)
    (add-to-list 'treesit-extra-load-path "/usr/local/lib/")
    (add-to-list 'treesit-extra-load-path "~/.local/lib/"))

  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (julia . ("https://github.com/tree-sitter/tree-sitter-julia"))
          (R . ("https://github.com/r-lib/tree-sitter-r"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src" "typescript"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))))
  :config
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(provide 'use-tree-sitter)
