;; Appearance settings -*- lexical-binding: t -*-

;; TODO: move to ivy module
;; (use-package all-the-icons-ivy
;;   :disabled
;;   :config
;;   (setq all-the-icons-ivy-file-commands
;;       '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir))
;;   (all-the-icons-ivy-setup))
;;
;; Custom theme load
(defun snug/disable-all-themes ()
  "Disable all enabled themes"
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

;; Theme hooks
(defvar snug/theme-hooks nil
  "Association list with theme names and hooks for them.")

(defun snug/add-theme-hook (theme hook)
  (add-to-list 'snug/theme-hooks (cons theme hook)))

;; TODO: add func to remove hook

(defun snug/load-theme-advice (f theme &optional no-confirm no-enable &rest args)
  "Advice `load-theme':

1. Disables enabled themes for a clean slate.
2. Calls functions registered using `snug/add-theme-hook'."
  (unless no-enable
    (snug/disable-all-themes))
  (prog1
      (apply f theme no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme snug/theme-hooks)
        (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme :around #'snug/load-theme-advice)

;; Theme settings
;; Setup my theme
(defvar snug-custom-theme 'kaolin-ocean
  "Default custom theme for snug-emacs.")

(use-package hl-todo
  :defer t
  :config
  (setq hl-todo-keyword-faces
        `(("IMPORTANT" . ,(face-foreground 'error))
          ("TODO"      . ,(face-foreground 'hl-todo))
          ("FIXME"     . ,(face-foreground 'hl-todo))
          ("MAYBE"     . ,(face-foreground 'warning))
          ("EXPLORE"   . ,(face-foreground 'warning))
          ("NOTE"      . ,(face-foreground 'success)))))


(use-package autothemer)

(use-package kaolin-themes
  :elpaca nil
  :after autothemer
  :load-path "local/emacs-kaolin-themes"
  :config
  (setq kaolin-themes-hl-line-colored nil
        kaolin-themes-git-gutter-solid t
        kaolin-themes-underline-wave t
        kaolin-themes-modeline-border nil
        ;; kaolin-themes-modeline-padded 2
        kaolin-themes-bold t
        kaolin-themes-org-scale-headings nil
        kaolin-themes-distinct-metakeys nil)

  ;; (setq kaolin-valley-light-alt-bg t)
  ;; (setq kaolin-valley-dark-alt-bg nil)

  ;; (setq kaolin-ocean-alt-bg t
  ;;       kaolin-light-alt-bg t)

  ;; (setq kaolin-themes-distinct-company-scrollbar t)
  ;; (setq kaolin-themes-distinct-fringe nil)
  ;; (setq kaolin-themes-italic-comments t)

  ;; (setq kaolin-themes-comments-style 'normal)
  ;; (setq kaolin-themes-comments-style 'alt)
  ;; (setq kaolin-themes-comments-style 'contrast)

  ;; Set default theme
  (add-hook 'after-make-frame-functions
            (lambda (_frame)
              (load-theme snug-custom-theme t)
              (global-hl-todo-mode t)))

  (unless (daemonp)
    (load-theme snug-custom-theme t))

  ;; (setq kaolin-themes-treemacs-modeline t)

  ;; (setq kaolin-themes-treemacs-icons nil)
  (kaolin-treemacs-theme)

  ;; Highlight t and nil in elisp-mode
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("\\<\\(nil\\|t\\)\\>" . 'kaolin-themes-boolean))))

;; TODO: do we need that?
(use-package pos-tip
  :defer t
  :config
  (setq pos-tip-border-width 1
        pos-tip-internal-border-width 2))
        ;; pos-tip-background-color (face-background 'tooltip)
        ;; pos-tip-foreground-color (face-foreground 'tooltip)))

;; Set default font
;; (add-to-list 'default-frame-alist '(font . "Victor Mono-13"))
;; (add-to-list 'default-frame-alist '(font . "D2Coding ligature-13"))
;; (add-to-list 'default-frame-alist '(font . "Writer-12"))
(add-to-list 'default-frame-alist '(font . "Cascadia Code-12.5"))

;; TODO conflicts with separedit package
(use-package ligature
  ;; :disabled t
  :elpaca (:repo "https://github.com/mickeynp/ligature.el")
  :hook (prog-mode . ligature-mode)
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  ;; (ligature-set-ligatures 'prog-mode '("-->" "->" "==" "<=" ">=" "/="))
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       ))
                                       ;; "\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  ;; (global-ligature-mode t)
  )


(use-package prog-mode
  :elpaca nil
  :init
  (setq prettify-symbols-unprettify-at-point t))

;; Set up fixed-pitch (monospace) and normal font
(defun snug/setup-fonts ()
  (custom-theme-set-faces
   'user
   '(fixed-pitch ((t (:family "D2Coding" :weight normal))))
   ;;  '(variable-pitch ((t (:family "Crimson Text" :height 165 :weight normal))))
   ;; '(variable-pitch ((t (:family "XCharter" :height 150 :weight normal))))
   ;; '(variable-pitch ((t (:family "Writter" :height 150 :weight normal))))
   ;; '(variable-pitch ((t (:family "Vollkorn" :height 135 :weight normal))))
   ;; '(variable-pitch ((t (:family "Libre Baskerville" :height 140 :weight normal))))
   '(variable-pitch ((t (:family "Iosevka Etoile" :height 125 :weight normal))))
   )
  )

(add-hook 'after-init-hook #'snug/setup-fonts)

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
  :elpaca nil
  :hook (prog-mode . hl-line-mode)
  :config
  ;; Highlight line only in current window
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

;; Disable scroll bars
;; (defun snug/disable-scroll-bars (frame)
;;   (modify-frame-parameters frame
;;                            '((vertical-scroll-bars . nil)
;;                              (horizontal-scroll-bars . nil))))

;; Set margin to Emacs frame
;; (add-to-list 'default-frame-alist '(internal-border-width . 4))

;; Add margins inside windows
(setq-default left-margin-width 1
              right-margin-width 1)

;; (add-hook 'after-make-frame-functions 'snug/disable-scroll-bars)

;; Hide default UI stuff
;; (when (fboundp 'tooltip-mode)
;;   (tooltip-mode -1) ; relegate tooltips to echo area only
;;   )

(setq show-help-function nil) ;; hide :help-echo text

;; (menu-bar-mode -1)
;; (when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
;; (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Setup cursor
;; Enable cursor blinking
(blink-cursor-mode t)
(setq blink-matching-paren t)

;; Disable startup/splash screen
(setq initial-scratch-message nil
      inhibit-startup-echo-area-message t
      inhibit-splash-screen t
      inhibit-startup-message t)

;; Startup echo message
(defalias 'display-startup-echo-area-message #'ignore)

;; Disable cursor in non selected windows
(setq-default cursor-in-non-selected-windows nil)

;;; Packages

;; Highlight numbers
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; Highlight defined Emacs Lisp symbols in source code
(use-package highlight-defined
  :commands (highlight-defined-mode)
  :hook (emacs-lisp-mode . highlight-defined-mode)
  :config
  (setq highlight-defined-face-use-itself t))

;; Highlight parenthess
(use-package paren
  :elpaca nil
  :hook (prog-mode . show-paren-mode)
  :config
  (setq show-paren-style 'parenthesis
                show-paren-when-point-inside-paren t
                show-paren-delay 0.1)
  (show-paren-mode t))

;; Highlight quoted symbols
(use-package highlight-quoted
  :commands (highlight-quoted-mode)
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;; (use-package highlight-symbol
;;   :diminish highlight-symbol-mode
;;   :hook (prog-mode . highlight-symbol-mode)
;;   :config (setq highlight-symbol-idle-delay 0.3))

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
  :hook ((prog-mode . indent-guide-mode)
         (evil-visual-state-entry . snug/disable-indent-guide)
         (evil-visual-state-exit  . indent-guide-mode))
  :config
  (defun snug/disable-indent-guide ()
    (interactive)
    (indent-guide-mode -1)))

;; Line numbering
(use-package nlinum
  :if (version< emacs-version "26.0")
  :hook ((prog-mode text-mode) . nlinum-mode)
  :config
  (setq nlinum-format "%5d ")
  (setq nlinum-highlight-current-line t))

(use-package display-line-numbers
  :elpaca nil
  :if (not (version< emacs-version "26.0"))
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-grow-only t))
  ;; MAYBE
  ;; (setq display-line-numbers-width-start t)

;; Highligh;; TODO
;; Highlight surrounding parentheses in Emacs
;; (use-package highlight-parentheses
;;   :config
;;   (global-highlight-parentheses-mode))

(use-package page-break-lines
  :disabled t
  :hook (elpaca-after-init . turn-on-page-break-lines-mode))

;; Default solaire-mode config
(use-package solaire-mode
  :after (kaolin-themes)
  ; :hook
  ; ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  ;; (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
  (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist)

  (solaire-global-mode +1)
  ;; (solaire-mode-swap-bg)

  ;; Treemacs fix
  ;; (with-eval-after-load 'treemacs
  ;;   (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
  ;;   (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist))
  )


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

;; (add-hook 'elpaca-after-init-hook #'remap-echo-background)
;; (add-hook 'echo-area-clear-hook #'remap-echo-background)

;; Hide mode-line in certain buffers
(use-package hide-mode-line
  :hook ((magit-status-mode . hide-mode-line-mode)
         (dashboard-mode    . hide-mode-line-mode)
         (helpful-mode      . hide-mode-line-mode)))

(use-package focus
  :commands (focus-mode))

(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

;; (use-package rainbow-mode
;;   :hook (((elpaca-after-init
;;            text-mode
;;            org-mode
;;            css-mode
;;            html-mode
;;            prog-mode). rainbow-mode))
;;   :diminish rainbow-mode)

(use-package mixed-pitch
  :commands (mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t))

(provide 'env-face)
