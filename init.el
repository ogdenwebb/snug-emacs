;;;; 0rdy Emacs config with Evil

(require 'package)
;; (setq package-enable-at-startup nil)   ; To prevent initialising twice
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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
(require 'env-check)
(require 'env-company)
(require 'env-evil)
(require 'env-face)
(require 'env-fu)
(require 'env-ivy)
(require 'env-maps)
(require 'env-modeline)
(require 'env-plugins)
(require 'env-sp)

;; Specific modules and major modes
(require 'use-eshell)
(require 'use-grep)
(require 'use-multiedit)
(require 'use-vcs)

(require 'use-clj)
(require 'use-org)
(require 'use-web)

;; (~ ^ . ^ ~)
