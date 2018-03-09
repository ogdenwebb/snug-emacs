;; -*- lexical-binding: t -*-

;; Custom theme load
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

;; Theme settings
;; List of nice themes
;; noctilux
;; bliss
;; boron
;; base16-default-dark
;; danneskjold-theme
;; flatland-theme
;; seti -- only basic faces

;; Load my theme
(use-package kaolin-themes
  ;; Delete the following line if you use MELPA package
  :load-path "dev/emacs-kaolin-themes"
  :init
  (use-package autothemer
    :ensure t)
  :config

  (setq kaolin-hl-line-colored t)
  (setq kaolin-git-gutter-solid t)
  (setq kaolin-wave t)
  (setq kaolin-bold nil)
  ;; (setq kaolin-italic-comments t)

  ;; Set default theme
  ;; (defun load-my-theme (frame)
  ;;   (with-selected-frame frame
  ;;     (load-theme 'kaolin-eclipse t)))

  ;; (if (daemonp)
  ;;     (add-hook 'after-make-frame-functions #'load-my-theme)
  ;;   (load-theme 'kaolin-eclipse t))

  ;; (load-theme 'kaolin-galaxy t)
  ;; (load-theme 'kaolin-aurora t)
  (load-theme 'kaolin-valley-dark t)

  ;; Highlight t and nil in elisp-mode
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("\\<\\(nil\\|t\\)\\>" . 'kaolin-boolean))))

;; Set default font
;; `default' face is not set properly
(add-to-list 'default-frame-alist '(font . "Roboto Mono-11.5"))
;; (add-to-list 'default-frame-alist '(font . "Iosevka-13"))
;; (add-to-list 'default-frame-alist '(font . "Fira Mono-12"))
;; (add-to-list 'default-frame-alist '(font . "Fira Code-12"))

;; Set the fringe size
(setq-default left-fringe-width  6)
(setq-default right-fringe-width 8)

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
(global-hl-line-mode 1)

;; Disable scroll bars
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))

(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;; Hide default UI stuff
(tooltip-mode -1) ; relegate tooltips to echo area only
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
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; Highlight defined Emacs Lisp symbols in source code
(use-package highlight-defined
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))

;; Highlight parenthess
(use-package paren
  :ensure t
  :config
  (show-paren-mode 1)        ; Automatically highlight parenthesis pairs
  ;; (setq show-paren-style 'expression)
  ;; (setq show-paren-style 'expression)
  (setq show-paren-delay 0.1))

;; Highlight quoted symbols
(use-package highlight-quoted
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

;; Dim innactive windows
;; (use-package auto-dim-other-buffers
;;   :config
;; (auto-dim-other-buffers-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
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
  :ensure t
  :config
  (indent-guide-global-mode))

;; Line numbering
(use-package nlinum
  :ensure t
  :init
  (setq nlinum-format "%5d ")
  (setq nlinum-highlight-current-line t)
  (add-hook 'prog-mode-hook 'nlinum-mode)
  (add-hook 'text-mode-hook 'nlinum-mode))

;; Icons
(use-package all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
             all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon
             all-the-icons-install-fonts)
  :ensure t)

;; Highlight TODO and FIXME
(use-package fic-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fic-mode))

;; TODO
;; Highlight surrounding parentheses in Emacs
;; (use-package highlight-parentheses
;;   :config
;;   (global-highlight-parentheses-mode))



(provide 'env-face)
