;;;; Elmax - Emacs configuration with Evil

;; (setq debug-on-error t)

;; General configation
(load (concat user-emacs-directory "boot/boot-prep"))

;; TODO: exclude either symlink or real file from recentf
;; setup with profiles: minimal, dev, etc
;; use-latex
;; Deal with defer t and defer .1
;; package restart-emacs
;; TODO: read featurep func

;; TODO: company-box
;; TODO: make region more distinct/colored

;; TODO: tests
;; TODO: maps with russian layout in input

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
