;;;; Elmax - Emacs configuration with Evil
;; Kull Wahad! Kull Wahad! Kull Wahad!

;; General configation

;; TODO: find a prefix key for major mode related maps
;; TODO: add env/use-media for twitter, reddit, etc
;; el-get
;; TODO (!!!!!!) parinfer is very slow on big files
;; (??) slime
;;
;; https://github.com/wcsmith/evil-args
;; https://github.com/ninrod/evil-replace-with-char/

;; org-mode https://github.com/tashrifsanil/org-easy-img-insert/
;; https://github.com/rexim/org-cliplink/
;; https://github.com/xuchunyang/grab-x-link/

;; (setq debug-on-error t)

(require 'package)
(setq package-enable-at-startup nil   ; To prevent initialising twice
      package--init-file-ensured t)

(setq package-user-dir (concat user-emacs-directory "elpa")
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")))

;; See log while loading
;; (setq use-package-verbose t)
;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc
;; (eval-and-compile
;;   (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
  (eval-when-compile
    ;; (require 'package)
    (package-initialize)
    ;; Install use-package if not installed yet.
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    ;; (require 'use-package)
    ;; (setq use-package-always-ensure t)
    (let ((package-user-dir-real (file-truename package-user-dir)))
      ;; The reverse is necessary, because outside we mapc
      ;; add-to-list element-by-element, which reverses.
      (nreverse (apply #'nconc
           ;; Only keep package.el provided loadpaths.
           (mapcar #'(lambda (path)
                   (if (string-prefix-p package-user-dir-real path)
                   (list path)
                     nil))
               load-path))))))


;; Enable use-package
(eval-when-compile
  (require 'use-package))

(load (concat user-emacs-directory "env/env-boot"))

(elmax/init
  env-common
  env-maps

  env-evil
  env-check
  env-company
  env-face
  ;; env-dashboard
  env-ivy
  env-plugins
  env-lisp
  env-local
  use-vcs

  ;; Mode-line
  modeline-common

  ;; Specific modules and major modes
  use-eshell
  use-grep

  ;; use-clj
  use-nim
  use-org
  use-web
  use-py
  use-js
  use-elixir

  env-fu)

;; (~ ^ . ^ ~)
