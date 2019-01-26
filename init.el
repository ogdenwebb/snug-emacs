;;;; Snug - Emacs configuration with Evil

;; (setq debug-on-error t)

;; General configation
(require 'snug (concat user-emacs-directory "snug/snug"))

(snug/init
  env-maps

  env-ivy
  ;; env-helm ; only for testing purpose

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
  ;; modeline-common
  modeline-doom

  ;; Specific modules and major modes
  use-eshell
  use-filetype
  use-grep
  use-sp ; smartparens
  use-lsp
  use-multiedit

  use-cc          ; c/c++
  use-cl          ; common lisp
  use-clj         ; clojure
  use-crystal
  use-calendar
  use-elixir
  use-haskell
  use-go
  use-js          ; javascript
  ;; use-nim
  ;; use-ocaml
  use-org         ; basic org setup
  use-org-capture ; capture things with org-mode
  use-py          ; python
  use-perl6       ;
  use-rust
  use-tools       ; support for external apps such as colorpicker
  use-vcs         ; version control
  use-writter     ; write in Emacs with a couple of tea
  use-yasnippet
  use-web)

;; (~ ^ . ^ ~)
