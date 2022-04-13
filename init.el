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
  ;; TODO: bind reset/ivy-repeat for vertico
  user-maps
)

;; user-settings

;; Customize echo area

;; (defface kaolin-themes-echo-area nil
;;   "Face to highlight boolean values"
;;   :group 'kaolin-themes)

;; (custom-theme-set-faces
;;  'user
;;  `(kaolin-themes-echo-area ((t (:inherit 'variable-pitch :height 0.95))))
;;  )


;; ;; Attach a face to the echo area in order to style it differently.
;; (dolist (buffer-name '(" *Echo Area 0*"
;;                        " *Echo Area 1*"))
;;   (with-current-buffer (get-buffer-create buffer-name)
;;     (setq-local face-remapping-alist
;;                 '((default kaolin-themes-echo-area)))))


;; (use-package hierarchy)
;; (use-package json-navigator
;;   :after hierarchy)


;; Simple symbol highlighting package for Emacs that performs well with large files.
;; (use-package idle-highlight-mode
;;   :straight (:host gitlab :repo "ideasman42/emacs-idle-highlight-mode"))

;; (use-package frog-jump-buffer)

;; Asynchronous Fuzzy Finder for Emacs
;; (use-package  affe
;;   :commands (affe-find affe-grep)
;;   :config
;;   ;; (with-eval-after-load 'orderless
;;     ;; (defun affe-orderless-regexp-compiler (input _type)
;;     ;;   (setq input (orderless-pattern-compiler input))
;;     ;;   (cons input (lambda (str) (orderless--highlight input str))))
;;     ;; (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

;;   (when (executable-find "fd")
;;     (setq affe-find-command "fd --color=never -i -E .git -H"))
;;   )

;; (use-package which-key)

;; Gnus
;; (use-package gnus
;;   :config
;;    (setq gnus-select-method '(nntp "news.gnus.org"))
;;   ;; (add-to-list 'gnus-secondary-select-methods '(nntp "localhost"))
;;   (add-to-list 'gnus-secondary-select-methods '(nntp "news.gnus.org"))
;;   (add-to-list 'gnus-secondary-select-methods '(nnml ""))
;;   )

;; (use-package csv-mode)

;; (use-package hierarchy
;;   :straight nil)

;; (~ ^ . ^ ~)
