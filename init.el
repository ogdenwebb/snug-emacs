;;;; Elmax - Emacs configuration with Evil

;; (setq debug-on-error t)

;; General configation
;; TODO: find a better name
(load (concat user-emacs-directory "env/env-prep"))

;; TODO: exclude either symlink or real file from recentf
;; setup with profiles: minimal, dev, etc
;; tools

(elmax/init
  env-settings
  env-maps

  env-evil
  env-company
  env-ivy
  ;; env-helm

  env-face
  ;; env-dashboard

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
  use-js
  use-nim
  use-org
  use-py
  use-web

  env-fu)

;; (~ ^ . ^ ~)
