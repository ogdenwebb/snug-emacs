;;;; 0rdy Emacs config with Evil

(require 'package)
;; (setq package-enable-at-startup nil)   ; To prevent initialising twice
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (eval-when-compile
;;   (require 'use-package))

;; General configation
(load (concat user-emacs-directory "env/env-base"))

(require 'env-common)
(require 'env-face)
(require 'env-ivy)
(require 'env-modeline)
(require 'env-maps)
(require 'env-plugins)
(require 'env-fu)
(require 'env-evil)
(require 'env-sp)
(require 'env-check)
(require 'env-company)

;; Module config

(require 'use-org)
(require 'use-vcs)
(require 'use-eshell)
(require 'use-web)
(require 'use-grep)
(require 'use-multiedit)

;; (~ ^ . ^ ~)
