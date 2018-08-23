;;;; Elmax - Emacs configuration with Evil

;; (setq debug-on-error t)

;; General configation
(require 'boot-prep (concat user-emacs-directory "boot/boot-prep"))

(elmax/init
  env-settings
  env-maps

  env-ivy
  ;; env-helm

  env-evil

  env-face
  ;; env-dashboard

  env-behavior
  env-plugins
  ;; env-dev

  env-check
  env-lisp
  env-local

  ;; Autocompletion
  completion-company ; TODO: refact
  ;; completion-lsp ; TODO: lsp isn't only completion

  ;; Mode-line
  modeline-common

  ;; Specific modules and major modes
  use-eshell
  use-filetype
  use-grep
  use-sp

  use-cl ; common lisp
  use-clj
  use-crystal
  use-elixir
  use-haskell
  use-js
  use-nim
  use-ocaml
  use-org
  use-py
  use-rust
  use-tools ; support for external apps
  use-vcs
  use-web)

;; (~ ^ . ^ ~)
