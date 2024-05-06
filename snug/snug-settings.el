;;; Snug settings file -*- lexical-binding: t -*-

;; Snug setting options

(defgroup snug nil
  "Snug-emacs custom options."
  :group 'emacs)

;; values: full, enable and nil
(defcustom snug-with-posframe nil
  "Enable posframe packages to display flycheck messages, company completion popup and etc."
  :type 'boolean
  :group 'snug)

(defcustom snug-with-lsp nil
  "Enable Language Server Protocol (LSP)."
  :type 'boolean
  :group 'snug)

(defcustom snug-with-eldoc-box t
  "Enable eldoc-box package to display eldoc information in childframe."
  :type 'boolean
  :group 'snug)

(defvar snug-user-shell 'zsh
  "Define user shell, e.g. bash, zsh, etc.")

(defcustom snug-default-indent-width 4
  "Set default indentation width. By default is 4."
  :group 'snug)

(defcustom snug-default-completion-system 'vertico
  "Set default (preferable) completion system, e.g. ivy, helm or selectrum."
  :group 'snug)

;; TODO:
;; (defcustom snug-org-headline-rescale nil)

;;; Emacs general setings

;; Set UTF-8 as default encoding
;; (set-charset-priority 'unicode)
;; (prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (setq buffer-file-coding-system 'utf-8
;;       locale-coding-system 'utf-8)

;; Disable 'wrote file' message
;; (setq save-silently t)

;; Replace the region while insert
(use-package delsel
  :elpaca nil
  :hook (elpaca-after-init . delete-selection-mode))

;; Enable mouse within terminal
(when (not window-system)
  (xterm-mouse-mode t))

;; Enable system clipboard in terminal Emacs
;; See: https://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; (setq select-enable-clipboard t)

;; (unless window-system
;;   (when (getenv "DISPLAY")
;;     ;; Callback for when user cuts
;;     (defun xsel-cut-function (text &optional push)
;;       ;; Insert text to temp-buffer, and "send" content to xsel stdin
;;       (with-temp-buffer
;;         (insert text)
;;         (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
;;     ;; Call back for when user pastes
;;     (defun xsel-paste-function()
;;       (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
;;         (unless (string= (car kill-ring) xsel-output)
;;           xsel-output)))
;;     ;; Attach callbacks to hooks
;;     (setq interprogram-cut-function 'xsel-cut-function)
;;     (setq interprogram-paste-function 'xsel-paste-function)))

;; Various Emacs settings
;; Set variables defined in C source code
(use-package emacs
  :elpaca nil
  :hook (eval-expression-minibuffer-setup . show-paren-mode)
  :config
  (setq frame-resize-pixelwise t  ; Resize frame pixelwise
        ;; frame-inhibit-implied-resize t
        ;; Disable bell
        visible-bell nil
        ring-bell-function 'ignore
        ;; Set frame title
        frame-title-format '(:eval
                             (concat (if (buffer-file-name)
                                         (abbreviate-file-name (buffer-file-name))
                                       "%b")
                                     " - Emacs"
                                     ))
        default-directory "~/"

        ;; Scroll settings
        scroll-margin                   0 ;  Add a margin when scrolling vertically
        hscroll-margin                  0
        ;; scroll-step                     7
        ;; hscroll-step                    7
        scroll-conservatively           101
        auto-window-vscroll nil
        ;; scroll-preserve-screen-position 'always
        mac-mouse-wheel-smooth-scroll    nil

        ;; Disable system-wide dialogs
        use-file-dialog nil
        use-dialog-box nil                ; Avoid GUI dialogs
        x-gtk-use-system-tooltips nil     ; Do not use GTK tooltips


        x-underline-at-descent-line t ; Underline looks a bit better when drawn lower
        ;; underline-minimum-offset 0

        ;; Disable lockfiles
        create-lockfiles nil

        ;; Disable mouse popup menu in mode-line
        mode-line-default-help-echo nil

        ;; Accelerate scrolling with the trade-off of sometimes delayed accurate fontification.
        ;; fast-but-imprecise-scrolling t
        )


  (setq-default tab-width snug-default-indent-width
                indent-tabs-mode nil

                line-spacing 1 ; Increase line space for better readability
                fill-column 80

                x-stretch-cursor nil
                visible-cursor nil
                highlight-nonselected-windows nil

                bidi-display-reordering nil    ; Disable bidirectional text for tiny performance boost
                indicate-buffer-boundaries nil ;  Don't show where buffer starts/ends
                indicate-empty-lines nil

                ;; Truncate lines
                truncate-lines t
                truncate-partial-width-windows nil

                mouse-yank-at-point t ; Yank at point rather than pointer

                apropos-do-all t) ; Better apropos

  (pixel-scroll-precision-mode t))

(use-package minibuffer
  :elpaca nil
  :hook (minibuffer-setup-hook . minibuffer-electric-default-mode)
  :config
  (setq read-answer-short t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t

        ;; Resize mini windows
        resize-mini-windows t
          ;; Enable recursive minibuffers, i.e. you can use M-x inside M-x
        enable-recursive-minibuffers t))

;; simple.el
(use-package simple
  :elpaca nil
  :config
  (setq-default yank-pop-change-selection t
                ;; Disable key bindging suggeestions
                suggest-key-bindings t
                kill-whole-line t	; Kill line including '\n'
                eval-expression-print-level nil
                set-mark-command-repeat-pop t

                async-shell-command-buffer 'new-buffer
                backward-delete-char-untabify-method 'hungry

                track-eol t ; Keep cursor at end of lines.
                line-move-visual nil ; To be required by track-eol

                ;; Update UI less frequently
                idle-update-delay 1.0
                ;; TODO: move to jit-lock
                jit-lock-defer-time 0
                )
  )

;; TODO: sort
(setq-default sentence-end-double-space nil
              custom-safe-themes t
              ;; custom-search-field nil
              )

;; TODO: sort
(setq
      ;; checkdoc-spellcheck-documentation-flag t
      ;; comint-input-ignoredups  t
      ;; comint-process-echoes  t
      comint-prompt-read-only  t
      ;; comint-scroll-to-bottom-on-input   ’this
      ;; completions-format  ’vertical
      )


;; `tty-run-terminal-initialization' is *tremendously* slow for some
;; reason. Disabling it completely could have many side-effects, so we
;; defer it until later.
(unless (display-graphic-p)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
    (defun snug-init-tty-h ()
      (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t))))


;; Compilation
(use-package compile
  :elpaca nil
  :config
  (setq compilation-always-kill t
        compilation-scroll-output t
        compilation-ask-about-save nil
        compilation-skip-threshold 2))

;; Replacing yes/no with y/n.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set tab length
(setq tab-stop-list (number-sequence 2 120 2))

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
;; (setq backward-delete-char-untabify-method 'hungry)

;; http://stackoverflow.com/questions/354490/preventing-automatic-change-of-default-directory
;; (add-hook 'find-file-hook
;;           (lambda ()
;;             (setq default-directory command-line-default-directory)))

;; Whitespace
(use-package whitespace
  :elpaca nil
  :defer t
  :config
  (setq-default whitespace-line-column 80
                ;; NOTE: break compability with whitespace-cleanup
                ;; whitespace-style '(face spaces tabs newline space-mark tab-mark
                                        ;; newline-mark lines-tail empty))
  ))

;; Auto reload buffer if file was changed
(use-package autorevert
  :elpaca nil
  :hook (elpaca-after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-interval 10
        auto-revert-check-vc-info nil
        ;; global-auto-revert-non-file-buffers t
        ;; TODO: Try fix hangs:
        auto-revert-use-notify nil

        auto-revert-verbose nil))

;; VC settings
(setq vc-follow-symlinks t)
;; (remove-hook 'find-file-hooks 'vc-find-file-hook)
;; (setq vc-handled-backends nil)

;; Suppress ad-handle-definition warnings
(setq ad-redefinition-action 'accept)

;; Save last position in buffer
(use-package saveplace
  :elpaca nil
  :hook (elpaca-after-init . save-place-mode))

;; Save history
(use-package savehist
  :elpaca nil
  :hook (elpaca-after-init . savehist-mode)
  :config
  (setq savehist-autosave-interval nil     ; save on kill only
        history-delete-duplicates t
        history-length 300
        savehist-save-minibuffer-history t

        savehist-additional-variables
        '(kill-ring                        ; persist clipboard
          mark-ring global-mark-ring       ; persist marks
          search-ring regexp-search-ring)) ; persist searches
  )

(use-package bookmark
  :elpaca nil
  :defer t
  :config
  (setq bookmark-save-flag t))

;; File-related settings
(use-package files
  :elpaca nil
  :after no-littering
  :config
  (setq make-backup-files t
        backup-by-copying t      ; don't clobber symlinks
        backup-by-copying-when-linked t
        backup-directory-alist '(("." . "~/.cache/emacs/backup"))
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t ; use versioned backups
        auto-save-default nil
        ;; auto-save-list-file-name nil
        auto-save-list-file-prefix nil

        large-file-warning-threshold nil ; Disable warning about large files
        require-final-newline t
        find-file-visit-truename t

        ;; Don’t bother confirming killing processes
        confirm-kill-processes nil

        ;; y/n instead of yes/no when quitting
        confirm-kill-emacs 'y-or-n-p

        ;; No second pass of case-insensitive search over auto-mode-alist.
        auto-mode-case-fold nil
        ))

(use-package find-file
  :elpaca nil
  :defer t
  :config
  (setq  ff-quiet-mode t))
;; TODO
  ;; (put 'ff-search-directories
  ;;      'safe-local-variable
  ;;      (lambda (x) (cl-every #'stringp x))))

;; (setq auto-save-file-name-transforms
;; `((".*" "~/.cache/emacs/saves/" t)))


;; Set default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; Allow commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; TODO: add var
;; Auto-indent when pasting
;; (dolist (command '(yank yank-pop))
;;   (eval `(defadvice ,command (after indent-region activate)
;;            (and (not current-prefix-arg)
;;                 (let ((mark-even-if-inactive transient-mark-mode))
;;                   (indent-region (region-beginning) (region-end) nil))))))

;; Use zsh
(setq shell-file-name "/bin/bash")
;; (setq shell-command-switch "-ic")

;; Save bookmarks when kill emacs
;; (add-hook 'kill-emacs-hook 'bookmark-save)

;; ;;; Code to replace exec-path-from-shell
;; ;; Need to create file in $HOME/.emacs.d/.local/env
;; ;; use this command to create the file  `printenv > $HOME/.emacs.d/.local/env'
;; (defconst my-local-dir (concat user-emacs-directory ".local/"))

;; (defconst my-env-file (concat my-local-dir "env"))

;; (defun my-load-envvars-file (file &optional noerror)
;;   "Read and set envvars from FILE.
;; If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
;; unreadable. Returns the names of envvars that were changed."
;;   (if (not (file-readable-p file))
;;       (unless noerror
;;         (signal 'file-error (list "Couldn't read envvar file" file)))
;;     (let (envvars environment)
;;       (with-temp-buffer
;;         (save-excursion
;;           (insert "\n")
;;           (insert-file-contents file))
;;         (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
;;           (push (match-string 1) envvars)
;;           (push (buffer-substring
;;                  (match-beginning 1)
;;                  (1- (or (save-excursion
;;                            (when (re-search-forward "^\\([^= ]+\\)=" nil t)
;;                              (line-beginning-position)))
;;                          (point-max))))
;;                 environment)))
;;       (when environment
;;         (setq process-environment
;;               (append (nreverse environment) process-environment)
;;               exec-path
;;               (if (member "PATH" envvars)
;;                   (append (split-string (getenv "PATH") path-separator t)
;;                           (list exec-directory))
;;                 exec-path)
;;               shell-file-name
;;               (if (member "SHELL" envvars)
;;                   (or (getenv "SHELL") shell-file-name)
;;                 shell-file-name))
;;         envvars))))

;; (when (and (or (display-graphic-p)
;;                (daemonp))
;;            (file-exists-p my-env-file))
;;   (my-load-envvars-file my-env-file))
;; ;;; Code to replace exec-path-from-shell

;; Native-comp setting for Emacs 28+
(if (and (not (version< emacs-version "28.0")) (featurep 'nativecomp))
    (use-package comp
      :elpaca nil
      :config
      (setq-default comp-async-compilation t
                    ;; native-comp-deferred-compilation nil
                    ;; Disable warning
                    native-comp-async-report-warnings-errors nil)))


;; Suppress compile messages
(when (and (version< emacs-version "28.0") (featurep 'nativecomp)
           (define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
             (let ((obsolete-name (pop ll))
                   (current-name (pop ll))
                   (when (if ll (pop ll) "1"))
                   (docstring (if ll (pop ll) nil)))
               (list obsolete-name current-name when docstring)))))


(provide 'snug-settings)
