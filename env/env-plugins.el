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


;; A minor mode that guesses the indentation offset
(use-package dtrt-indent
  :disabled t
  :defer t
  :hook
  (prog-mode . dtrt-indent-mode)
  :config
  ;; Hide messages from dtrt-indent
  (setq dtrt-indent-verbosity snug-debug-mode))

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
    ("q"   nil "cancel" :color blue))

  :config
  ;; Projectile settings
  (setq projectile-verbose  nil
        ;; projectile-ignored-project-function  ’file-remote-p
        ;; projectile-require-project-root   t
        ;; projectile-switch-project-action  ’projectile-dired
        ))

;; TODO: enable
;; (use-package backup-each-save
;;   :hook (after-save-hook . backup-each-save))

;; ;;TODO: ensure backups are being created
;; (use-package backup-walker
;;   :commands backup-walker-start)

;; Intelligently call whitespace-cleanup before buffers are saved.
(use-package whitespace-cleanup-mode
  :defer t
  :hook (after-init . whitespace-cleanup-mode))

;; Quickrun
(use-package quickrun
  :defer t
  :commands (quickrun quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region))

;; Move region or line
(use-package drag-stuff
  :defer t
  :commands (drag-stuff-left drag-stuff-up drag-stuff-down drag-stuff-right))
;; :config (drag-stuff-global-mode 1))

;;;###autoload
(defun snug/set-no-fringes ()
  "Remove fringes in window. Mainly uses as hook."
  (when (display-graphic-p)
    (setq left-fringe-width 0)
    (setq right-fringe-width 0)))

(use-package bug-hunter
  :defer t
  :commands (bug-hunter-init-file bug-hunter-file))

;; (use-package bug-reference
;;   :ensure nil
;;   :hook ((prog-mode . bug-reference-prog-mode)
;;          (text-mode . bug-reference-mode)))

(use-package treemacs
  :defer t
  :commands (treemacs treemacs-create-theme treemacs-create-icon treemacs-load-theme)
  :config
  (setq-default treemacs-fringe-indicator-mode nil)

  ;; (pcase (cons (not (null (executable-find "git")))
  ;;              (not (null (executable-find "python3"))))
  ;;   (`(t . t)
  ;;    (treemacs-git-mode 'extended))
  ;;   (`(t . _)
  ;;    (treemacs-git-mode 'simple)))

  ;; (setq treemacs-git-mode t)

  (setq treemacs-show-hidden-files nil
        treemacs-silent-refresh t
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
                      "C-h"  'evil-window-left
                      "C-j"  'evil-window-down
                      "C-k"  'evil-window-up
                      "C-l"  'evil-window-right))

;; Treemacs extensions
(use-package treemacs-evil
  ;; :defer t
  :after (treemacs evil))

(use-package treemacs-projectile
  ;; :defer t
  :after (treemacs projectile))

(use-package treemacs-magit
  ;; :defer t
  :after (treemacs magit))

;; Allows to use treemacs icons in dired buffers
(use-package treemacs-icons-dired
  :defer t
  :hook (dired-mode . treemacs-icons-dired-mode))

;; Show tooltip help for keybindings
(use-package which-key
  :disabled t
  :commands (which-key-mode)
  :config
  (which-key-setup-side-window-bottom))

;; Eldoc
(use-package eldoc
  :defer t
  :straight nil
  :hook (prog-mode-hook . eldoc-mode)
  :config
  (global-eldoc-mode -1)
  (setq eldoc-idle-delay 0.3))

;; TODO: Disable in terminal
(use-package eldoc-box
  :if snug-with-eldoc-box
  :defer t
  :hook (eldoc-mode . eldoc-box-hover-mode))

(use-package google-translate
  :defer t
  :config)

(use-package google-translate-smooth-ui
  :defer t
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
  :defer t
  :disabled t
  :commands (restart-emacs))

;; TODO: https://github.com/EFLS/zetteldeft
(use-package deft
  :defer t
  :commands (deft)
  :config
  (setq deft-extensions '("txt" "tex" "org")
        deft-default-extension "org"
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        deft-org-mode-title-prefix t
        deft-directory "~/Drive/org"
        ;; converts the filter string into a readable file-name using kebab-case:
        deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase))
        deft-recursive t))

;; Better help
(use-package helpful
  :if (>= emacs-major-version 25)
  :defer 1
  ;; :bind (([remap describe-function] . helpful-callable)
  ;;        ([remap describe-variable] . helpful-variable)
  ;;        ([remap describe-key] . helpful-key))
  :preface
  (defun counsel-helpful-keymap-describe ()
    "Select keymap with ivy, display help with helpful"
    (interactive)
    (ivy-read "describe keymap: " (let (cands)
                                    (mapatoms
                                     (lambda (x)
                                       (and (boundp x) (keymapp (symbol-value x))
                                            (push (symbol-name x) cands))))
                                    cands)
              :require-match t
              :history 'counsel-describe-keymap-history
              :sort t
              :preselect (ivy-thing-at-point)
              :keymap counsel-describe-map
              :caller 'counsel-helpful-keymap-describe
              :action (lambda (map-name)
                        (helpful-variable (intern map-name)))))
  :init
  (with-eval-after-load 'counsel
    (setq-default counsel-describe-function-function #'helpful-callable
                  counsel-describe-variable-function #'helpful-variable))

  (defalias 'describe-key 'helpful-key)
  )

(use-package frameshot
  :disabled t
  :hook (after-init . frameshot-mode)
  :config
  (setq frameshot-default-setup t))

;; Simple but effective sorting and filtering for Emacs.
(use-package prescient
  :defer t)
  ;; Change save file location
  ;; (setq prescient-save-file (locate-user-emacs-file "cache/prescient-save.el"))
  ;; Use fuzzy matching by default
  ;; (setq prescient-filter-method 'fuzzy))
    ;; Enable persistent history
  ;; (prescient-persist-mode))

;; TODO: setup for counsel-ag
;; (use-package ivy-prescient
;;   ;; :disabled t
;;   :defer t
;;   ;; :after (ivy)
;;   :hook (ivy-mode . ivy-prescient-mode))

(use-package company-prescient
  :defer t
  :hook ((company-mode global-company-mode) . company-prescient-mode))

(use-package avy :defer t)
(use-package ace-window :defer t)
;;; Libraries
(use-package async      :defer t)
;; (use-package anaphora      :defer t)
;; (use-package apiwrap       :defer t)
;; (use-package asoc          :defer t)
;; (use-package button-lock   :defer t)
(use-package bind-key      :defer t)
;; (use-package ctable        :defer t)
;; (use-package concurrent    :defer t)
(use-package dash            :defer t)
(use-package dash-functional :defer t)
;; (use-package deferred      :defer t)
;; (use-package el-mock       :defer t)
(use-package elisp-refs
  :defer t
  :commands
  (elisp-refs-function elisp-refs-macro elisp-refs-variable elisp-refs-special elisp-refs-symbol))
;; (use-package epc           :defer t)
(use-package epl           :defer t)
;; (use-package esxml         :defer t)
(use-package embrace         :defer t)
(use-package f             :defer t)
;; (use-package fn            :defer t)
;; (use-package flx           :defer t)
;; (use-package fringe-helper :defer t)
;; (use-package fuzzy         :defer t)
(use-package gh            :defer t)
;; (use-package ghub          :defer t)
;; (use-package ghub+         :defer t)
;; (use-package graphql       :defer t)
(use-package ht            :defer t)
;; (use-package kv            :defer t)
;; (use-package list-utils    :defer t)
(use-package logito        :defer t)
(use-package loop          :defer t)
(use-package lv          :defer t)
;; (use-package m-buffer      :defer t)
;; (use-package makey         :defer t)
(use-package marshal       :defer t)
(use-package memoize       :defer t)
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
(use-package pkg-info      :defer t)
(use-package popup         :defer t)
;; (use-package popup-pos-tip :defer t)
;; (use-package popwin        :defer t)
;; (use-package request       :defer t)
;; (use-package rich-minority :defer t)
(use-package s                :defer t)
(use-package shut-up          :defer t)
(use-package seq              :defer t)
(use-package shrink-path      :defer t)
;; (use-package simple-httpd  :defer t)
;; (use-package spinner       :defer t)
(use-package transient        :defer t)
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
(use-package posframe
  :defer t)
(use-package lv
  :defer t)

(use-package alert
  :defer t
  :preface
  (defun alert-after-compilation-finish (buf result)
    "Use `alert' to report compilation RESULT if BUF is hidden."
    (when (buffer-live-p buf)
      (unless (catch 'is-visible
                (walk-windows (lambda (w)
                                (when (eq (window-buffer w) buf)
                                  (throw 'is-visible t))))
                nil)
        (alert (concat "Compilation " result)
               :buffer buf
               :category 'compilation)))))

(use-package compile
  :defer t
  :hook (compilation-finish-functions . alert-after-compilation-finish))

;; (use-package ansi-color
;;   :ensure nil
;;   :hook (compilation-filter . colorize-compilation-buffer)
;;   :preface
;;   (autoload 'ansi-color-apply-on-region "ansi-color")
;;   (defun colorize-compilation-buffer ()
;;     (let ((inhibit-read-only t))
;;       (ansi-color-apply-on-region (point-min) (point-max)))))

(use-package unfill
  :defer t
  :commands (unfill-region unfill-paragraph unfill-toggle))

;; https://gitlab.com/emacs-stuff/indent-tools
(use-package indent-tools
  :defer t
  :bind (("C-c TAB" . indent-tools-hydra/body)))

;; Provides a command which searches for unicode characters by name
(use-package list-unicode-display
  :defer t
  :commands (list-unicode-display))

(use-package link-hint
  :defer t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

;; ;; Make scripts executable automatically.
;; (use-package executable
;;   :ensure nil
;;   :hook
;;   ((after-save .
;;     executable-make-buffer-file-executable-if-script-p)))

;; (use-package string-inflection
;;   :bind (("C-c r r" . string-inflection-all-cycle)
;;          ("C-c r c" . string-inflection-camelcase)
;;          ("C-c r l" . string-inflection-lower-camelcase)
;;          ("C-c r u" . string-inflection-underscore)
;;          ("C-c r k" . string-inflection-kebab-case)
;;          ("C-c r J" . string-inflection-java-style-cycle)))


(provide 'env-plugins)
