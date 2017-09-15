;;;; 0rdy Emacs config with Evil

;; TODO:
;; add debug/minimal mode
;; (setq max-lisp-eval-depth 10000)
;; (setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
;; Emacs is slow with large(1k+ lines) files
;; Completion in :ielm
;; TODO: (??) Fix slow first start
;; USE PROFILER

(setq debug-on-error t)

(require 'package)
(setq package-enable-at-startup nil)   ; To prevent initialising twice
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; General configation
(load (concat user-emacs-directory "env/env-boot"))

(require 'env-common)
(require 'env-maps)
(require 'env-check)
(require 'env-company)
(require 'env-evil)
(require 'env-face)
(require 'env-fu)
(require 'env-ivy)
(require 'env-modeline)
(require 'env-plugins)
(require 'env-sp)

;; (require 'env-dev)

;; Specific modules and major modes
;; TODO: use-ocaml or use-ml
(require 'use-eshell)
(require 'use-grep)
;; (require 'use-multiedit)
(require 'use-vcs)

;; (require 'use-clj)
;; (require 'use-elm)
(require 'use-nim)
(require 'use-org)
(require 'use-web)

;; (~ ^ . ^ ~)
