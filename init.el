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

;; General configation
(load (concat user-emacs-directory "env/env-boot"))

;; TODO:
;; https://github.com/tam17aki/ace-isearch
;; Fix telephone-line. See messages.

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
