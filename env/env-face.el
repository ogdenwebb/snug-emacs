;; -*- lexical-binding: t -*-

;; Custom theme load
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

;; Theme settings
;; Load my theme
(defvar elmax/custom-theme 'kaolin-valley-dark
  "Default custom theme.")

(use-package autothemer
  :ensure t)

(use-package kaolin-themes
  :after autothemer
  ;; Delete or comment the following line if you use MELPA package
  :load-path "dev/emacs-kaolin-themes"
  :config
  (setq kaolin-themes-hl-line-colored t
        kaolin-themes-git-gutter-solid t
        kaolin-themes-underline-wave t
        kaolin-themes-bold nil)
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

  (load-theme elmax/custom-theme t)

  ;; Highlight t and nil in elisp-mode
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("\\<\\(nil\\|t\\)\\>" . 'kaolin-themes-boolean))))

;; Set default font
(add-to-list 'default-frame-alist '(font . "Roboto Mono-11.5"))
;; (add-to-list 'default-frame-alist '(font . "Hasklig-13"))
;; (add-to-list 'default-frame-alist '(font . "Fira Mono-12"))
;; (add-to-list 'default-frame-alist '(font . "Fira Code-12"))
;; (add-to-list 'default-frame-alist '(font . "Input Mono-11"))
;; (add-to-list 'default-frame-alist '(font . "Noto Mono-12"))

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
;; (global-hl-line-mode 1)
(add-hook 'prog-mode-hook #'hl-line-mode)

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
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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
  :config
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; Highlight defined Emacs Lisp symbols in source code
(use-package highlight-defined
  :commands (highlight-defined-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode))

;; Highlight parenthess
(use-package paren
  :config
  (show-paren-mode 1)
  ;; (setq show-paren-style 'expression)
  (setq show-paren-delay 0.1))

;; Highlight quoted symbols
(use-package highlight-quoted
  :commands (highlight-quoted-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

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
  :init
  (add-hook 'prog-mode-hook #'indent-guide-mode))

;; Line numbering
(use-package nlinum
  :if (version< emacs-version "26.0")
  :config
  (add-hook 'prog-mode-hook 'nlinum-mode)
  (add-hook 'text-mode-hook 'nlinum-mode)
  (setq nlinum-format "%5d ")
  (setq nlinum-highlight-current-line t))

(use-package display-line-numbers
  :if (not (version< emacs-version "26.0"))
  :init
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'text-mode-hook 'display-line-numbers-mode)
  :config
  (setq display-line-numbers-grow-only t))

;; Icons
(use-package all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
             all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon
             all-the-icons-install-fonts)
  :ensure t)

;; Highlight TODO and FIXME
(use-package fic-mode
  :ensure t
  :defer .1
  :config
  (add-hook 'prog-mode-hook #'fic-mode))

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
  (add-hook 'imenu-list-hook #'solaire-mode)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer))
  ;; (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  ;; (solaire-mode-swap-bg))

;; (defun no-fringes-in-minibuffer ()
;;   "Disable fringes in the minibuffer window."
;;   (set-window-fringes (minibuffer-window) 0 0 nil))

;; (add-hook 'minibuffer-setup-hook #'no-fringes-in-minibuffer)

(provide 'env-face)
