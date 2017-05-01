;;;; 0rdy Emacs config with Evil

(require 'package)
(setq package-enable-at-startup nil)   ; To prevent initialising twice
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; TODO:
;; Local package dev

;; TODO:
;; Disable autoindent two empty lines
;; indent-relative-maybe
;; = key is evil-indent command
;; indent-region
;; https://github.com/syl20bnr/spacemacs/issues/5010

;; TODO:
;; multi-term
;; Electric indent mode
;; Fix RET in term/eshell mode

;; Install https://github.com/zk-phi/highlight-stages
;; add faces ^

;; https://github.com/iqbalansari/mu4e-alert

;; General configation
(load (concat user-emacs-directory "env/env-boot"))

(require 'env-common)
(require 'env-check)
(require 'env-company)
(require 'env-maps)
(require 'env-evil)
(require 'env-face)
(require 'env-fu)
(require 'env-ivy)
(require 'env-modeline)
(require 'env-plugins)
(require 'env-sp)

;; Specific modules and major modes
(require 'use-eshell)
(require 'use-grep)
;; (require 'use-multiedit)
(require 'use-vcs)

(require 'use-clj)
;; (require 'use-elm)
(require 'use-nim)
(require 'use-org)
(require 'use-web)

;; (~ ^ . ^ ~)
