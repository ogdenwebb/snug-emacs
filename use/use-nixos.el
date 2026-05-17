;; Nix language and NixOS related configuration

;; An Emacs major mode for editing Nix expressions.
(use-package nix-mode
  ;; :disabled t
  :mode "\\.nix\\'")

(use-package nixfmt
  :hook (nix-mode-hook . nixfmt-on-save-mode))

(provide 'use-nixos)
