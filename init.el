;;;; Snug - Emacs configuration with Evil

;; TODO: add remap for ivy module
;; TODO: add remap for helm module


;; TODO:
;; company-tooltip-align-annotations t
;; company-tooltip-width-grow-only t
;; company-abort-manual-when-too-short t

;;;  TODO: take care of elisp compile warning in developer mode

;; General configation
(require 'snug (concat user-emacs-directory "snug/snug"))

(snug/init
  env-maps

  ;; MAYBE: rename to completion-system?
  ;; env-ivy
  ;; env-helm
  env-vertico

  env-evil
  env-editing ;; pairs, code navigation, etc
  ;; TODO: evil binding for puni

  env-face

  ;; env-dashboard

  env-behavior
  env-lookup
  env-plugins
  ;; env-dev

  env-check
  env-hydra
  env-lisp
  ;; env-sessions

  ;; Autocompletion
  ;; use-completion-company
  use-completion-corfu

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
  ;; use-sp ; smartparens
  use-lsp ; lsp has   lsp-treemacs
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
  use-nim
  ;; use-ocaml
  use-org            ; basic org setup and appearance
  use-org-capture    ; capture things with org-mode
  ;; use-org-workflow    ; org based workflow
  ;; TODO: clean both kill-ring and system clipboard
  ;; MAYBE: make kill-new avoids clipboard?
  ;; https://stackoverflow.com/questions/17127009/how-to-disable-emacs-evil-selection-auto-copies-to-clipboard

  ;; TODO: set or advice
  ;; (setq select-enable-clipboard nil)
  use-pass          ; password-store
  ;; use-py          ; python
  ;; use-raku
  ;; use-repl
  use-rust
  use-tags
  use-tools          ; support for external apps such as colorpicker
  ;; use-tabs           ; UI tabs

  use-vcs            ; version control
  use-writter        ; write in Emacs with a couple of tea
  use-yasnippet
  use-web
  use-wm             ; easy way to manage Emacs windows

  ;; User specific settings
  ;; TODO: bind reset/ivy-repeat for vertico
  user-maps
)

;; user-settings

;; (~ ^ . ^ ~)
