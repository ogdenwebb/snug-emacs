;;;; Elmax - Emacs configuration with Evil
;; Kull Wahad! Kull Wahad! Kull Wahad!

;; (setq debug-on-error t)

;; General configation
;; TODO: find a better name
(load (concat user-emacs-directory "env/env-init"))

(elmax/init
  env-settings
  env-maps

  env-evil
  env-company
  env-ivy

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
  use-eshell
  use-grep

  use-clj
  use-nim
  use-org
  use-web
  use-py
  use-js
  use-elixir

  env-fu)

;; (~ ^ . ^ ~)
