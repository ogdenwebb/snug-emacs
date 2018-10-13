;; -*- lexical-binding: t -*-

;; Custom theme load
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

;; Theme settings
;; Load my theme
(defvar snug/custom-theme 'kaolin-valley-dark
  "Default custom theme.")

(use-package autothemer)

(use-package kaolin-themes
  :after autothemer
  ;; Delete or comment the following line if you use MELPA package
  :load-path "dev/emacs-kaolin-themes"
  :config
  (setq kaolin-themes-hl-line-colored nil
        kaolin-themes-git-gutter-solid t
        kaolin-themes-underline-wave t
        kaolin-themes-bold nil)

  ;; (setq kaolin-valley-light-alt-bg t)
  ;; (setq kaolin-themes-distinct-company-scrollbar t)
  ;; (setq kaolin-themes-italic-comments nil)

  ;; (setq kaolin-themes-comments-style 'normal)

  ;; Set default theme
  ;; (defun load-my-theme (frame)
  ;;   (with-selected-frame frame
  ;;     (load-theme 'kaolin-eclipse t)))

  ;; (if (daemonp)
  ;;     (add-hook 'after-make-frame-functions #'load-my-theme)
  ;;   (load-theme 'kaolin-eclipse t))

  (load-theme snug/custom-theme t)

  (kaolin-treemacs-theme)

  ;; Highlight t and nil in elisp-mode
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("\\<\\(nil\\|t\\)\\>" . 'kaolin-themes-boolean))))

;; Set default font
;; (add-to-list 'default-frame-alist '(font . "Roboto Mono-12:style=regular"))
;; (add-to-list 'default-frame-alist '(font . "Hasklig-12.5"))
;; (add-to-list 'default-frame-alist '(font . "Fira Mono-12"))
;; (add-to-list 'default-frame-alist '(font . "Fira Code-12.5:style=regular"))
;; (add-to-list 'default-frame-alist '(font . "Input Mono-11"))
;; (add-to-list 'default-frame-alist '(font . "Noto Mono-12")) ; <-
;; (add-to-list 'default-frame-alist '(font . "iosevka-13"))
(add-to-list 'default-frame-alist '(font . "D2Coding-13"))

;; Set the fringe size
(setq-default left-fringe-width  6
              right-fringe-width 8)

;; Disable newline markers in fringe
;; (setq overflow-newline-into-fringe nil)
(setf (cdr (assq 'truncation fringe-indicator-alist)) '(nil nil))

(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000])
(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000])

;; Need to show fringe in vertical split
(setq-default fringes-outside-margins t)


;; Highlight current line
(use-package hl-line
  :hook (prog-mode . hl-line-mode))

;; Disable scroll bars
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))

(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;; Hide default UI stuff
(tooltip-mode -1) ; relegate tooltips to echo area only
(setq show-help-function nil)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Enable cursor blinking
(blink-cursor-mode 1)

;; Disable startup/splash screen
(setq initial-scratch-message nil)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(defalias 'display-startup-echo-area-message #'ignore)

;; Disable cursor in non selected windows
(setq-default cursor-in-non-selected-windows nil)

;;;; Packages

;; Highlight numbers
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; Highlight defined Emacs Lisp symbols in source code
(use-package highlight-defined
  :commands (highlight-defined-mode)
  :hook (emacs-lisp-mode . highlight-defined-mode))

;; Highlight parenthess
(use-package paren
  :hook (after-init . show-paren-mode)
  :config
  ;; (setq show-paren-style 'expression)
  (setq show-paren-delay 0.1))

;; Highlight quoted symbols
(use-package highlight-quoted
  :commands (highlight-quoted-mode)
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :hook ((prog-mode clojure-mode) . rainbow-delimiters-mode))

;; Show indent line
;; TODO: (??) disable in swiper
;; (use-package highlight-indent-guides
;;   :init
;;   (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;;   :config
;;   (setq highlight-indent-guides-method 'character)
;;   ;; Indent character samples: | ┆ ┊
;;   (setq highlight-indent-guides-character ?\┆))

(use-package indent-guide
  :commands (indent-guide-mode indent-guide-global-mode)
  :hook (prog-mode . indent-guide-mode))

;; Line numbering
(use-package nlinum
  :if (version< emacs-version "26.0")
  :hook ((prog-mode text-mode) . nlinum-mode)
  :config
  (setq nlinum-format "%5d ")
  (setq nlinum-highlight-current-line t))

(use-package display-line-numbers
  :if (not (version< emacs-version "26.0"))
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-grow-only t))

;; Icons
(use-package all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
                                   all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon
                                   all-the-icons-install-fonts))

;; Highlight TODO and FIXME
(use-package fic-mode
  :defer .1
  :hook (prog-mode . fic-mode))

;; TODO
;; Highlight surrounding parentheses in Emacs
;; (use-package highlight-parentheses
;;   :config
;;   (global-highlight-parentheses-mode))

(use-package solaire-mode
  :disabled
  ;; :hook ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  :config
  ;; (setq solaire-mode-remap-modeline nil)
  (add-hook 'treemacs-mode-hook #'solaire-mode)
  (add-hook 'imenu-list-major-mode-hook #'solaire-mode)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer))
;; (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
;; (solaire-mode-swap-bg))

;; (defun no-fringes-in-minibuffer ()
;;   "Disable fringes in the minibuffer window."
;;   (set-window-fringes (minibuffer-window) 0 0 nil))

;; (add-hook 'minibuffer-setup-hook #'no-fringes-in-minibuffer)

;; (defun remap-echo-background ()
;;   (with-current-buffer " *Echo Area 0*" (face-remap-add-relative 'default '(:background "red")))
;;   (with-current-buffer " *Echo Area 1*" (face-remap-add-relative 'default '(:background "red"))))


;; TODO:
;; https://groups.google.com/forum/#!topic/gnu.emacs.help/-_x4WLfQPps
;; (defun remap-echo-background ()
;;   (dolist (b (buffer-list))
;;     (when (or (string= (buffer-name b) " *Echo Area 0*")
;;               (string= (buffer-name b) " *Echo Area 1*")
;;               (minibufferp b))
;;       (with-current-buffer b
;;         (face-remap-add-relative 'default '(:background "red"))))))

;; (add-hook 'after-init-hook #'remap-echo-background)
;; (add-hook 'echo-area-clear-hook #'remap-echo-background)

;; Hide mode-line in certain buffers
(use-package hide-mode-line
  :hook (magit-status-mode . hide-mode-line-mode))


(provide 'env-face)
