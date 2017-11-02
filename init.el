;;;; Elmax - Emacs configuration with Evil
;; Kull Wahad! Kull Wahad! Kull Wahad!


;; General configation

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load (concat user-emacs-directory "env/env-boot"))

(require 'env-common)
(require 'env-maps)
(require 'env-check)
(require 'env-company)
(require 'env-evil)
(require 'env-face)
;; TODO: rename
(require 'env-fu)
(require 'env-ivy)
(require 'env-modeline)
(require 'env-plugins)
;; (require 'env-sp)
(require 'env-lisp)
(require 'env-dev)

;; Specific modules and major modes
;; (require 'use-multiedit)
(require 'use-eshell)
(require 'use-grep)
(require 'use-vcs)

;; (require 'use-clj)
;; (require 'use-elm)
;; TODO:
;; (require 'use-nim)
(require 'use-org)
(require 'use-web)

;; (~ ^ . ^ ~)
