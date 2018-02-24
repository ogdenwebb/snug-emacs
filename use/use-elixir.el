(use-package elixir-mode
  :mode (("\\.ex\\'" . elixir-mode)
         ("\\.exs\\'" . elixir-mode)
         ("\\.elixir2\\'" . elixir-mode)))

(use-package alchemist
  :after elixir-mode
  :hook (elixir-mode . alchemist-mode)
  :config
  (setq alchemist-test-status-modeline nil))

(use-package alchemist-company
  :after (company elixir))

(provide 'use-elixir)
