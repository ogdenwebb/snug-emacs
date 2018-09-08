;; TODO: split  -*- lexical-binding: t -*-

(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                           (let ((pkg (cadr (assq name where))))
                             (when pkg
                               (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (format "Upgrade %d package%s (%s)? "
                       (length upgrades)
                       (if (= (length upgrades) 1) "" "s")
                       (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))

;; Project management
(use-package projectile
  :defer 1
  :config
  (projectile-global-mode t))

;; TODO: enable
;; (use-package backup-each-save
;;   :hook (after-save-hook . backup-each-save))

;; ;;TODO: ensure backups are being created
;; (use-package backup-walker
;;   :commands backup-walker-start)

;; Quickrun
(use-package quickrun
  :ensure t
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region))

;; Move region or line
(use-package drag-stuff
  :ensure t
  :commands (drag-stuff-left drag-stuff-up drag-stuff-down drag-stuff-right))
;; :config (drag-stuff-global-mode 1))

;; Yasnippet
;; TODO:
;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (yas-global-mode 1))

(defun my-treemacs-no-fringes ()
  "Remove fringes in treemacs. They get reset each time you select the neotree
pane and are highlighted incorrectly when used with `solaire-mode'."
  (when (display-graphic-p)
    (set-window-fringes nil 0 0)))

(defun my-treemacs-hook ()
  (face-remap-add-relative 'hl-line `(:background "nil" :foreground ,(face-foreground 'font-lock-preprocessor-face)))
  (setq-local line-spacing 1)
  (my-treemacs-no-fringes))

(add-hook 'treemacs-mode-hook 'my-treemacs-hook)

(use-package treemacs
  :defer t
  ;; :load-path "dev/treemacs"
  :commands (treemacs)
  :config
  (use-package treemacs-evil
    :after (treemacs evil))

  (use-package treemacs-projectile
    :after (treemacs projectile))

  ;; Disable mode-line in treemacs buffer
  (defun treemacs--setup-mode-line ()
    (setq mode-line-format nil))

  ;; (pcase (cons (not (null (executable-find "git")))
  ;;              (not (null (executable-find "python3"))))
  ;;   (`(t . t)
  ;;    (treemacs-git-mode 'extended))
  ;;   (`(t . _)
  ;;    (treemacs-git-mode 'simple)))

  (setq treemacs-git-mode nil)

  (setq treemacs-show-hidden-files nil
        treemacs-width 30
        treemacs-indentation 2
        treemacs-indentation-string " "
        ;; treemacs-indentation-string (propertize "|" 'face 'font-lock-comment-face)
        treemacs-follow-after-init t
        treemacs-filewatch-mode t
        treemacs-tag-follow-mode t
        treemacs-file-event-delay 1000)

  (setq treemacs-icon-root-png
        (format " %s " (all-the-icons-material "subject" :v-adjust -0.2 :height 1.4
                                               :face 'font-lock-variable-name-face))

        treemacs-icon-open-png
        (format "%s " (all-the-icons-material "folder_open" :v-adjust -0.2 :height 1.15 'font-lock-doc-face))

        treemacs-icon-closed-png
        (format "%s " (all-the-icons-material "folder" :v-adjust -0.2 :height 1.15))

        treemacs-icon-tag-open-png
        (all-the-icons-faicon "chevron-down" :v-adjust 0.1)

        treemacs-icon-tag-closed-png
        (all-the-icons-faicon "location-arrow" :v-adjust 0.1)

        treemacs-icon-tag-node-open-png
        (format "%s " (all-the-icons-faicon "chevron-down"  :height 0.75 :face 'font-lock-keyword-face))

        treemacs-icon-tag-node-closed-png
        (format "%s " (all-the-icons-faicon "location-arrow" :height 0.9  :face 'font-lock-keyword-face))

        treemacs-icon-tag-leaf-png
        (format "%s " (all-the-icons-faicon "tag" :height 0.9 :face 'font-lock-type-face))
        )

  (setq treemacs-icons-hash (make-hash-table :size 200 :test #'equal)
        treemacs-icon-fallback (format "%s " (all-the-icons-faicon "file-text-o"))
        treemacs-icon-text treemacs-icon-fallback)

  ;; TODO: mb add macro
  (treemacs-define-custom-icon (all-the-icons-alltheicon "csharp-line") "cs")
  (treemacs-define-custom-icon (all-the-icons-alltheicon "css3") "css")
  (treemacs-define-custom-icon (all-the-icons-alltheicon "git") "gitignore")
  (treemacs-define-custom-icon (all-the-icons-alltheicon "html5") "html" "htm")
  (treemacs-define-custom-icon (all-the-icons-alltheicon "java") "java")
  (treemacs-define-custom-icon (all-the-icons-alltheicon "javascript") "js")
  (treemacs-define-custom-icon (all-the-icons-alltheicon "python") "py")
  (treemacs-define-custom-icon (all-the-icons-alltheicon "rust") "rs")
  (treemacs-define-custom-icon (all-the-icons-alltheicon "haskell") "hs")
  (treemacs-define-custom-icon (all-the-icons-alltheicon "c") "c")
  (treemacs-define-custom-icon (all-the-icons-alltheicon "cplusplus") "cpp")
  (treemacs-define-custom-icon (all-the-icons-alltheicon "ruby-alt") "rb")
  (treemacs-define-custom-icon (all-the-icons-alltheicon "scala") "scala")
  (treemacs-define-custom-icon (all-the-icons-alltheicon "elixir") "ex" "exs")
  (treemacs-define-custom-icon (all-the-icons-alltheicon "erlang") "erl" "hrl")
  (treemacs-define-custom-icon (all-the-icons-fileicon "clisp") "lisp")
  (treemacs-define-custom-icon (all-the-icons-fileicon "go") "go")
  (treemacs-define-custom-icon (all-the-icons-fileicon "elisp") "el" "elc")
  (treemacs-define-custom-icon (all-the-icons-fileicon "julia") "jl")
  (treemacs-define-custom-icon (all-the-icons-fileicon "kotlin") "kt" "kts")
  (treemacs-define-custom-icon (all-the-icons-fileicon "hy") "hy")
  (treemacs-define-custom-icon (all-the-icons-fileicon "jsx2-alt") "jsx")
  (treemacs-define-custom-icon (all-the-icons-fileicon "ocaml") "ml" "mli")
  (treemacs-define-custom-icon (all-the-icons-fileicon "org") "org")
  (treemacs-define-custom-icon (all-the-icons-fileicon "php") "php")
  (treemacs-define-custom-icon (all-the-icons-fileicon "powershell") "sh" "zsh")
  (treemacs-define-custom-icon (all-the-icons-fileicon "typescript") "ts")
  (treemacs-define-custom-icon (all-the-icons-fileicon "nimrod") "nim" "nims")
  (treemacs-define-custom-icon (all-the-icons-fileicon "perl6") "pm6")
  (treemacs-define-custom-icon (all-the-icons-fileicon "tex") "tex")
  (treemacs-define-custom-icon (all-the-icons-fileicon "rst") "rst")
  (treemacs-define-custom-icon (all-the-icons-fileicon "vue") "vue")
  (treemacs-define-custom-icon (all-the-icons-octicon "markdown") "md" "markdown")
  (treemacs-define-custom-icon (all-the-icons-octicon "file-pdf") "pdf")
  (treemacs-define-custom-icon (all-the-icons-octicon "database") "sql")
  (treemacs-define-custom-icon (all-the-icons-material "style") "styles")

  (treemacs-define-custom-icon (all-the-icons-octicon "file-media")
                               "jpg" "jpeg" "png" "gif" "ico" "tif" "tiff" "svg" "bmp"
                               "psd" "ai" "eps" "indd" "mov" "avi" "mp4" "webm" "mkv"
                               "wav" "mp3" "ogg" "midi")

  (treemacs-define-custom-icon (all-the-icons-faicon "file-text-o")
                               "rst" "log" "txt" "CONTRIBUTE" "LICENSE" "README" "CHANGELOG")

  (treemacs-define-custom-icon (all-the-icons-faicon "cogs")
                               "conf" "cfg" "yaml" "yml" "json" "xml" "toml" "cson" "ini")

  (treemacs-define-custom-icon (all-the-icons-octicon "code")
                               "tpl" "erb" "mustache" "twig" "ejs" "mk" "haml" "pug" "jade")

  (treemacs-define-custom-icon (all-the-icons-octicon "file-zip")
                               "zip" "xz" "tar" "7z" "rar")

  :general
  (general-define-key :keymaps 'treemacs-mode-map
                      :states  '(normal visual treemacs)
                      ;; "g j"  'treemacs-next-neighbour
                      ;; "g k"  'treemacs-previous-neighbour
                      "g j"  'treemacs-next-project
                      "g k"  'treemacs-previous-project
                      "M-h"  'evil-window-left
                      "M-j"  'evil-window-down
                      "M-k"  'evil-window-up
                      "M-l"  'evil-window-right))

;; (use-package which-key
;;   :config
;;   (which-key-mode)
;;   (which-key-setup-side-window-bottom))

;; Simple Emacs minor mode for a nice writing environment.
(use-package olivetti
  :ensure t
  :commands (olivetti-mode olivetti-shrink olivetti-expand olivetti-toggle-hide-mode-line)
  :config
  (setq-default olivetti-body-width 80))

(use-package google-translate
  :defer t
  :config
  (use-package google-translate-smooth-ui
    :after google-translate
    :commands google-translate-smooth-translate)
  (setq google-translate-translation-directions-alist '(("en" . "ru"))))

(use-package imenu-list
  :commands (imenu-list-smart-toggle)
  :config
  ;; (imenu-list-minor-mode t)
  (setq-default imenu-list-mode-line-format nil)
  (setq imenu-list-mode-line-format nil
        imenu-list-focus-after-activation t
        imenu-list-size 32))

;; TODO: server restart
(use-package restart-emacs
  :commands (restart-emacs))

(use-package deft
  :commands (deft)
  :config
  (setq deft-extensions '("txt" "tex" "org")
        deft-directory "~/Drive/org"
        deft-recursive t))

;; (use-package helpful
;;   :ensure t
;;   :general
;;   (general-define-key :states 'normal
;;                       :prefix leader
;;                       "h k" 'helpful-key
;;                       "h v" 'helpful-variable
;;                       ;; "h f" 'helpful-function
;;                       "h f" 'helpful-callable
;;                       "h l" 'find-library
;;                       "h ." 'helpful-at-point))

(provide 'env-plugins)
