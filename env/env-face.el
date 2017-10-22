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
(use-package kaolin-theme
  ;; Comment the following line if you use MELPA package
  :load-path "themes/kaolin-theme"
  :init
  ;; Set default theme
  (defun load-my-theme (frame)
    ;; (select-frame frame)
    (with-selected-frame frame
      (load-theme 'kaolin-eclipse t)))

  (if (daemonp)
      (add-hook 'after-make-frame-functions #'load-my-theme)
    (load-theme 'kaolin-eclipse t))

  :config
  ;; TODO:
  ;; (custom-theme-set-faces
  ;;  'kaolin-eclipse
  ;;  '(org-done ((t (:foreground "dimgray" :bold t :strike-through t))))
  ;;  '(org-headline-done ((t (:foreground "dimgray" :bold nil :strike-through t)))))

  (setq kaolin-hl-line-colored t)
  ;; Highlight t and nil in elisp-mode
  (font-lock-add-keywords 'emacs-lisp-mode
    '(("\\<\\(nil\\|t\\)\\>" . 'kaolin-boolean))))

;; Set default font
;; TODO: Error: highlight-indent-guides cannot auto set faces:
;; `default' face is not set properly
(add-to-list 'default-frame-alist '(font . "Roboto Mono-11.5"))

;; (let ((default-font "Iosevka-12"))
;;   (assq-delete-all 'font default-frame-alist)
;;   (add-to-list 'default-frame-alist
;;          `(font . ,default-font))
;;   (set-frame-font default-font))

  ;; (set-face-attribute 'default t
  ;;                     :family "Roboto Mono for Powerline"
  ;;                     :height 110
  ;;                     :weight 'normal
  ;;                     :width  'normal))

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

;; Hide toolbar and menu
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Disable startup/splash screen
;; TODO: disable scrollbar-mode
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
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; Highlight defined Emacs Lisp symbols in source code
(use-package highlight-defined
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))

;; Highlight parenthess
(use-package paren
  :config
  (show-paren-mode 1)        ; Automatically highlight parenthesis pairs
  (setq show-paren-style 'parenthesis)
  ;; (setq show-paren-style 'expression)
  (setq show-paren-delay 0)) ; show the paren match immediately

;; Highlight quoted symbols
(use-package highlight-quoted
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

;; Dim innactive windows
;; (use-package auto-dim-other-buffers
;;   :config
;; (auto-dim-other-buffers-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
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

;; Line numbering
(use-package nlinum
    :init
    (setq nlinum-format "%4d ")
    (setq nlinum-highlight-current-line t)
    (add-hook 'prog-mode-hook 'nlinum-mode)
    (add-hook 'text-mode-hook 'nlinum-mode))

;; Dashboard
;; TODO: fix slow startup mb move it
;; (use-package dashboard
;;   :init
;;   (dashboard-setup-startup-hook)
;;   ;; Set the title
;;   (setq dashboard-banner-logo-title "I know no Evil")
;;   ;; Set the banner
;;   (setq dashboard-startup-banner 'logo)

;;   (setq dashboard-items
;;         '((recents   . 5)
;;           (bookmarks . 5)
;;           (projects  . 5))))

;; Icons
(use-package all-the-icons)

;; Highlight TODO and FIXME
(use-package fic-mode
  :config
  (add-hook 'prog-mode-hook 'fic-mode))

(provide 'env-face)
