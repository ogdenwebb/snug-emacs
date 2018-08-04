;;;; Elmax - Emacs configuration with Evil

;; (setq debug-on-error t)

;; kaolin-sunflower
;; TODO: rename boot-prep to bootstrap
;; TODO: color test
;; TODO: fix tab in terminal within org-mode

;; General configation
(require 'boot-prep (concat user-emacs-directory "boot/boot-prep"))

(elmax/init
  env-settings
  env-maps

  env-ivy
  ;; env-helm

  env-evil
  env-company ; TODO: refact

  env-face
  env-dashboard

  env-behavior
  env-plugins
  ;; env-dev

  env-check
  env-lisp
  env-local

  use-vcs

  ;; Mode-line
  modeline-common

  ;; Specific modules and major modes
  use-filetype
  use-eshell
  use-grep
  use-sp

  use-clj
  use-crystal
  use-elixir
  use-haskell
  use-js
  use-nim
  use-org
  use-ocaml
  use-py
  use-rust
  use-web

  use-tools ; support for external apps

  env-fu)

;; (~ ^ . ^ ~)
