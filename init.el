;;;; Snug - Emacs configuration with Evil

;; General configation
(require 'snug (concat user-emacs-directory "snug/snug"))

(snug/init
  env-maps

  ;; TODO: swiper-isearch
  env-ivy
  ;; env-helm

  ;; TODO: evil-shift-round
  ;; TODO: evil-shift-width for modes like web
  env-evil

  env-face
  env-face-pretty

  ;; env-dashboard

  env-behavior
  env-plugins
  ;; env-dev

  env-check
  env-lisp

  ;; Autocompletion
  use-completion-company

  ;; Mode-line
  modeline-common ; TODO: rework
  ;; modeline-doom

  ;; Specific modules and major modes
  use-dired
  use-eshell
  ;; use-diff
  use-filetype
  use-grep
  use-sp ; smartparens
  ;; use-lsp ; lsp has   lsp-treemacs
  ;; use-multiedit

  use-cc             ; c/c++
  ;; use-cl          ; common lisp
  ;; use-clj         ; clojure
  ;; use-crystal
  use-calendar
  ;; use-elixir
  use-haskell
  use-go
  use-lua
  ;; use-js          ; javascript
  ;; use-kotlin
  ;; use-nim
  ;; use-ocaml
  use-org            ; basic org setup
  use-org-capture    ; capture things with org-mode
  ;; use-py          ; python
  use-perl6
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
  ;; TODO: general has :properties :jump :repeat for maps
  ;; TODO: general-def
  user-maps)
;; user-settings

;; (~ ^ . ^ ~)
