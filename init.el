;;;; Elmax - Emacs configuration with Evil
;; Kull Wahad! Kull Wahad! Kull Wahad!

;; General configation
(load (concat user-emacs-directory "env/env-rc"))

(elmax/init
  env-common
  env-maps

  env-evil
  env-check
  env-company
  env-face
  ;; env-dashboard
  env-ivy
  env-plugins
  env-lisp
  env-local
  use-vcs

  ;; Mode-line
  modeline-common

  ;; Specific modules and major modes
  use-eshell
  use-grep

  ;; use-clj
  use-nim
  use-org
  use-web
  use-py
  use-js
  use-elixir

  env-fu)

;; (~ ^ . ^ ~)
