;; TODO: split  -*- lexical-binding: t -*-

;; (defun package-upgrade-all ()
;;   "Upgrade all packages automatically without showing *Packages* buffer."
;;   (interactive)
;;   (package-refresh-contents)
;;   (let (upgrades)
;;     (cl-flet ((get-version (name where)
;;                            (let ((pkg (cadr (assq name where))))
;;                              (when pkg
;;                                (package-desc-version pkg)))))
;;       (dolist (package (mapcar #'car package-alist))
;;         (let ((in-archive (get-version package package-archive-contents)))
;;           (when (and in-archive
;;                      (version-list-< (get-version package package-alist)
;;                                      in-archive))
;;             (push (cadr (assq package package-archive-contents))
;;                   upgrades)))))
;;     (if upgrades
;;         (when (yes-or-no-p
;;                (format "Upgrade %d package%s (%s)? "
;;                        (length upgrades)
;;                        (if (= (length upgrades) 1) "" "s")
;;                        (mapconcat #'package-desc-full-name upgrades ", ")))
;;           (save-window-excursion
;;             (dolist (package-desc upgrades)
;;               (let ((old-package (cadr (assq (package-desc-name package-desc)
;;                                              package-alist))))
;;                 (package-install package-desc)
;;                 (package-delete  old-package)))))
;;       (message "All packages are up to date"))))

;; Project management
(use-package projectile
  :defer 1
  :config
  (projectile-global-mode t)
  :hydra
  (hydra-projectile (:color teal :hint nil)
    "
       PROJECTILE: %(projectile-project-root)

    ^Find File^        ^Search/Tags^        ^Buffers^       ^Cache^                    ^Project^
    ^---------^        ^-----------^        ^-------^       ^-----^                    ^-------^
    _f_: file          _a_: ag              _i_: Ibuffer    _c_: cache clear           _p_: switch project
    _F_: file dwim     _g_: update gtags    _b_: switch to  _x_: remove known project
    _C-f_: file pwd    _o_: multi-occur   _s-k_: Kill all   _X_: cleanup non-existing
    _r_: recent file   ^ ^                  ^ ^             _z_: cache current
    _d_: dir
   "
    ("a"   projectile-ag)
    ("b"   projectile-switch-to-buffer)
    ("c"   projectile-invalidate-cache)
    ("d"   projectile-find-dir)
    ("f"   projectile-find-file)
    ("F"   projectile-find-file-dwim)
    ("C-f" projectile-find-file-in-directory)
    ("g"   ggtags-update-tags)
    ("s-g" ggtags-update-tags)
    ("i"   projectile-ibuffer)
    ("K"   projectile-kill-buffers)
    ("s-k" projectile-kill-buffers)
    ("m"   projectile-multi-occur)
    ("o"   projectile-multi-occur)
    ("p"   projectile-switch-project)
    ("r"   projectile-recentf)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("z"   projectile-cache-current-file)
    ("q"   nil "cancel" :color blue)))

;; TODO: enable
;; (use-package backup-each-save
;;   :hook (after-save-hook . backup-each-save))

;; ;;TODO: ensure backups are being created
;; (use-package backup-walker
;;   :commands backup-walker-start)

;; Quickrun
(use-package quickrun
  :commands (quickrun quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region))

;; Move region or line
(use-package drag-stuff
  :commands (drag-stuff-left drag-stuff-up drag-stuff-down drag-stuff-right))
;; :config (drag-stuff-global-mode 1))

;;;###autoload
(defun snug/set-no-fringes ()
  "Remove fringes in window. Mainly uses as hook."
  (when (display-graphic-p)
    (setq left-fringe-width 0)
    (setq right-fringe-width 0)))

(use-package bug-hunter
  :commands (bug-hunter-init-file bug-hunter-file))

(use-package treemacs
  :defer t
  :commands (treemacs)
  :config
  (setq-default treemacs-fringe-indicator-mode nil)

  ;; (pcase (cons (not (null (executable-find "git")))
  ;;              (not (null (executable-find "python3"))))
  ;;   (`(t . t)
  ;;    (treemacs-git-mode 'extended))
  ;;   (`(t . _)
  ;;    (treemacs-git-mode 'simple)))

  (setq treemacs-git-mode nil)

  (setq treemacs-show-hidden-files nil
        treemacs-width 30
        ;; treemacs-indentation 1
        ;; treemacs-indentation-string "  "
        ;; treemacs-indentation-string (propertize "|" 'face 'font-lock-comment-face)
        treemacs-follow-after-init t
        treemacs-filewatch-mode t
        treemacs-tag-follow-mode t
        treemacs-file-event-delay 1000)

  :general
  (general-define-key :keymaps 'treemacs-mode-map
                      :states  '(normal visual treemacs evil-treemacs)
                      ;; "g j"  'treemacs-next-neighbour
                      ;; "g k"  'treemacs-previous-neighbour
                      "g j"  'treemacs-next-project
                      "g k"  'treemacs-previous-project
                      "M-h"  'evil-window-left
                      "M-j"  'evil-window-down
                      "M-k"  'evil-window-up
                      "M-l"  'evil-window-right))

;; Treemacs extensions
(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-mode))

(use-package which-key
  :disabled t
  :commands (which-key-mode)
  :config
  (which-key-setup-side-window-bottom))

;; TODO: enable in prog-mode prolly
(use-package eldoc-box
  ;; :hook ((eglot--managed-mode . eldoc-box-hover-mode)
  ;;        (eglot--managed-mode . eldoc-box-hover-at-point-mode)))
  :config
  (add-hook 'prog-mode-hook 'eldoc-box-hover-mode))

(use-package google-translate
  :defer t
  :config)

(use-package google-translate-smooth-ui
  :straight nil
  ;; :after google-translate
  :commands google-translate-smooth-translate
  :config
  (setq google-translate-translation-directions-alist '(("en" . "ru"))))

(use-package imenu-list
  :hook (imenu-list-major-mode . snug/set-no-fringes)
  :commands (imenu-list-smart-toggle)
  :config
  ;; (imenu-list-minor-mode t)
  (setq-default imenu-list-mode-line-format nil)
  (setq imenu-list-mode-line-format nil
        imenu-list-focus-after-activation t
        imenu-list-size 32))

;; TODO: server restart
(use-package restart-emacs
  :disabled t
  :commands (restart-emacs))

;; TODO: https://github.com/EFLS/zetteldeft
(use-package deft
  :commands (deft)
  :config
  (setq deft-extensions '("txt" "tex" "org")
        deft-directory "~/Drive/org"
        deft-recursive t))

;; Better help
(use-package helpful
  :init
  (with-eval-after-load 'counsel
    (setq-default counsel-describe-function-function #'helpful-callable
                  counsel-describe-variable-function #'helpful-variable))
  :general
  (general-define-key :states 'normal
                      :prefix snug-leader
                      "h k" 'helpful-key
                      "h K" 'general-describe-keybindings
                      "h v" 'helpful-variable
                      ;; "h f" 'helpful-function
                      "h f" 'helpful-callable
                      "h l" 'find-library
                      "h ." 'helpful-at-point))

(use-package frameshot
  :disabled t
  :hook (after-init . frameshot-mode)
  :config
  (setq frameshot-default-setup t))

(use-package prescient
  :disabled
  :defer t)
  ;; Change save file location
  ;; (setq prescient-save-file (locate-user-emacs-file "cache/prescient-save.el"))
  ;; Use fuzzy matching by default
  ;; (setq prescient-filter-method 'fuzzy))
    ;; Enable persistent history
  ;; (prescient-persist-mode))

(use-package ivy-prescient
  :disabled t
  :after (ivy)
  :config
  (ivy-prescient-mode t))

(use-package company-prescient
  :disabled t
  :after (prescient company)
  :hook ((company-mode global-company-mode) . company-prescient-mode))


(use-package avy :defer t)
;;; Libraries
;; (use-package anaphora      :defer t)
;; (use-package apiwrap       :defer t)
;; (use-package asoc          :defer t)
;; (use-package button-lock   :defer t)
;; (use-package bind-key      :defer t)
;; (use-package ctable        :defer t)
;; (use-package concurrent    :defer t)
;; (use-package dash          :defer t)
;; (use-package deferred      :defer t)
;; (use-package el-mock       :defer t)
;; (use-package elisp-refs    :defer t)
;; (use-package epc           :defer t)
;; (use-package epl           :defer t)
;; (use-package esxml         :defer t)
;; (use-package f             :defer t)
;; (use-package fn            :defer t)
;; (use-package flx           :defer t)
;; (use-package fringe-helper :defer t)
;; (use-package fuzzy         :defer t)
;; (use-package gh            :defer t)
;; (use-package ghub          :defer t)
;; (use-package ghub+         :defer t)
;; (use-package graphql       :defer t)
;; (use-package ht            :defer t)
;; (use-package kv            :defer t)
;; (use-package list-utils    :defer t)
;; (use-package logito        :defer t)
;; (use-package loop          :defer t)
;; (use-package m-buffer      :defer t)
;; (use-package makey         :defer t)
;; (use-package marshal       :defer t)
;; (use-package names         :defer t)
;; (use-package noflet        :defer t)
;; (use-package oauth2        :defer t)
;; (use-package ov            :defer t)
;; (use-package packed        :defer t)
;; (use-package parent-mode   :defer t)
;; (use-package parsebib      :defer t)
;; (use-package parsec        :defer t)
;; (use-package peval         :defer t)
;; (use-package pfuture       :defer t)
;; (use-package pkg-info      :defer t)
;; (use-package popup         :defer t)
;; (use-package popup-pos-tip :defer t)
;; (use-package popwin        :defer t)
;; (use-package request       :defer t)
;; (use-package rich-minority :defer t)
;; (use-package s             :defer t)
;; (use-package simple-httpd  :defer t)
;; (use-package spinner       :defer t)
;; (use-package tablist       :defer t)
;; (use-package treepy        :defer t)
;; (use-package uuidgen       :defer t)
;; (use-package web           :defer t)
;; (use-package web-server    :defer t)
;; (use-package websocket     :defer t)
;; (use-package with-editor   :defer t)
;; (use-package xml-rpc       :defer t)
;; (use-package zoutline      :defer t)
;; (use-package map           :defer t)
;;
(provide 'env-plugins)
