;;;; Snug - Emacs configuration with Evil

;; (setq debug-on-error t)

;; General configation
(require 'snug (concat user-emacs-directory "snug/snug"))

(snug/init
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

  ;; Mode-line
  modeline-common

  ;; Specific modules and major modes
  use-eshell
  use-filetype
  use-grep
  use-sp ; smartparens
  ;; use-lsp

  use-cl ; common lisp
  use-clj
  use-crystal
  use-calendar
  use-elixir
  use-haskell
  use-js
  use-nim
  use-ocaml
  use-org
  use-org-capture
  use-py
  use-perl6
  use-rust
  use-tools ; support for external apps
  use-vcs
  use-writter
  use-web)

;; (~ ^ . ^ ~)
