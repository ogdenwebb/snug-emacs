;; TODO: split to basic plugin and dev plugin  -*- lexical-binding: t -*-
;; (use-package package-utils
;;   :ensure t
;;   :commands (package-utils-upgrade-all))

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
  :ensure t
  :config
  (projectile-mode +1))

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

(use-package treemacs
  :ensure t
  :defer t
  :commands (treemacs)
  :config
  (use-package treemacs-evil
    :after treemacs)

  ;; Disable mode-line in treemacs buffer
  (defun treemacs--setup-mode-line ()
      (setq mode-line-format nil))

  :general
  (general-define-key :keymaps 'treemacs-mode-map
                      :states  '(normal visual treemacs)
                      "g j"  'treemacs-next-neighbour
                      "g k"  'treemacs-previous-neighbour
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
  (setq imenu-list-mode-line-format nil)
  (setq imenu-list-size 32))

(use-package restart-emacs
  :commands (restart-emacs))

(use-package deft
  :commands (deft)
  :config
  (setq deft-extensions '("txt" "tex" "org"))
  (setq deft-directory "~/Drive/org")
  (setq deft-recursive t))

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
