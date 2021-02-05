;;;; Snug - Emacs configuration with Evil

;; General configation
(require 'snug (concat user-emacs-directory "snug/snug"))

(snug/init
  env-maps

  env-ivy
  ;; env-helm
  ;; env-selectrum

  env-evil

  env-face

  ;; env-dashboard

  env-behavior
  env-plugins
  ;; env-dev

  env-check
  env-hydra
  env-lisp
  ;; env-sessions

  ;; Autocompletion
  use-completion-company

  ;; Mode-line
  modeline-common
  ;; modeline-doom

  ;; Specific modules and major modes
  use-dired
  use-eshell
  ;; use-diff
  use-filetype
  use-format
  use-grep
  use-sp ; smartparens
  ;; use-lsp ; lsp has   lsp-treemacs
  ;; use-multiedit

  use-cc             ; c/c++
  ;; use-cl          ; common lisp
  ;; use-clj         ; clojure
  ;; use-crystal
  use-calendar       ; show date and events
  ;; use-elixir
  use-haskell
  use-ibuffer
  use-go
  use-lua
  use-js          ; javascript
  ;; use-kotlin
  ;; use-nim
  ;; use-ocaml
  use-org            ; basic org setup and appearance
  use-org-capture    ; capture things with org-mode
  ;; use-org-workflow    ; org based workflow
  ;; use-py          ; python
  ;; use-raku
  ;; use-rust
  use-tags
  use-tools          ; support for external apps such as colorpicker
  ;; use-tabs           ; UI tabs

  use-vcs            ; version control
  use-writter        ; write in Emacs with a couple of tea
  use-yasnippet
  use-web
  use-wm             ; easy way to manage Emacs windows

  ;; User specific settings
  user-maps)
;; user-settings

;; (~ ^ . ^ ~)
