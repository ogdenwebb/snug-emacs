;;; Snug settings file -*- lexical-binding: t -*-

;; Snug setting options

(defgroup snug nil
  "Snug-emacs custom options."
  :group 'emacs)

;; values: full, enable and nil
(defvar snug-with-posframe nil
  "Enable posframe packages to display flycheck messages, company completion popup and etc."
  :type 'boolean
  :group 'snug)

(defvar snug-with-lsp nil
  "Enable Language Server Protocol (LSP)."
  :type 'boolean
  :group 'snug)

(defvar snug-with-eldoc-box t
  "Enable eldoc-box package to display eldoc information in childframe."
  :type 'boolean
  :group 'snug)

;; TODO:
;; (defvar snug-org-headline-rescale nil)

;; Emacs general setings
;; Set UTF-8 as default encoding
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8
      locale-coding-system 'utf-8)

;; Disable 'wrote file' message
;; (setq save-silently t)

;; Replace the region while insert
(use-package delsel
  :straight nil
  :hook (after-init . delete-selection-mode))

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

;; Set frame title
(setq frame-title-format
   '(:eval
     (if (buffer-file-name)
         (abbreviate-file-name (buffer-file-name))
       "%b")))

(setq-default yank-pop-change-selection t
              x-stretch-cursor nil
              visible-cursor nil
              highlight-nonselected-windows nil
              ;; Disable key bindging suggeestions
              suggest-key-bindings t
              indicate-buffer-boundaries nil ;  Don't show where buffer starts/ends
              bidi-display-reordering nil ; Disable bidirectional text for tiny performance boost
              indicate-empty-lines nil
              sentence-end-double-space nil
              kill-whole-line t	; Kill line including '\n'
              apropos-do-all t ; Better apropos
              custom-safe-themes t
              ;; custom-search-field nil
              eval-expression-print-level nil
              ;; Resize mini windows
              resize-mini-windows t
              set-mark-command-repeat-pop t
              auto-window-vscroll nil
              )

;; TODO: sort
(setq async-shell-command-buffer 'new-buffer
      backward-delete-char-untabify-method 'hungry
      ;; checkdoc-spellcheck-documentation-flag t
      ;; comint-input-ignoredups  t
      ;; comint-process-echoes  t
      ;; comint-prompt-read-only  t
      ;; comint-scroll-to-bottom-on-input   ’this
      ;; completions-format  ’vertical
      )

;; Compilation
(setq compilation-always-kill t
      compilation-ask-about-save  nil
      compilation-skip-threshold  2
      )

;; Custom options for Emacs
(setq use-dialog-box nil                ; Avoid GUI dialogs
      x-gtk-use-system-tooltips nil     ; Do not use GTK tooltips
      large-file-warning-threshold nil ; Disable warning about large files
      track-eol t ; Keep cursor at end of lines.
      line-move-visual nil)	 ; To be required by track-eol


;; Replacing yes/no with y/n.
(defalias 'yes-or-no-p 'y-or-n-p)

;; y/n instead of yes/no when quitting
(setq confirm-kill-emacs 'y-or-n-p)

;; Indentation
(setq tab-width 4)

(setq-default indent-tabs-mode nil)

;; Set tab length
(setq tab-stop-list (number-sequence 2 120 2))
(with-eval-after-load 'evil
  (setq evil-shift-width tab-width))

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
;; (setq backward-delete-char-untabify-method 'hungry)

;; http://stackoverflow.com/questions/354490/preventing-automatic-change-of-default-directory
;; (add-hook 'find-file-hook
;;           (lambda ()
;;             (setq default-directory command-line-default-directory)))

;; Whitespace
(use-package whitespace
  :straight nil
  :defer t
  :config
  (setq-default whitespace-line-column 80
                whitespace-style '(face spaces tabs newline space-mark tab-mark
                                        newline-mark lines-tail empty)))

;; Auto reload buffer if file was changed
(use-package autorevert
  :straight nil
  :hook (after-init . global-auto-revert-mode))

;; Lines
(setq indicate-empty-lines t
      require-final-newline t)

;; Disable bell
(setq visible-bell nil
      ring-bell-function 'ignore)

;; Truncate lines
(setq-default truncate-lines t)

;; VC settings
(setq find-file-visit-truename t)
(setq vc-follow-symlinks t)
;; (remove-hook 'find-file-hooks 'vc-find-file-hook)
;; (setq vc-handled-backends nil)

;; Suppress ad-handle-definition warnings
(setq ad-redefinition-action 'accept)

;; Resize frame pixelwise
(setq frame-resize-pixelwise t)

;; Save last position in buffer
(use-package saveplace
  :straight nil
  :hook (after-init . save-place-mode))

;; Disable lockfiles
(setq create-lockfiles nil)

;; TODO: read
;; (setq auto-save-list-file-name nil)

;; Save history
(use-package savehist
  :config
  (savehist-mode t)
  :config
  (setq savehist-autosave-interval 60
        history-delete-duplicates t
        history-length 1000
        savehist-save-minibuffer-history t
        savehist-additional-variables '(search-ring
                                        kill-ring
                                        set-variable-value-history
                                        shell-command-history
                                        file-name-history
                                        regexp-search-ring)))

;; Backup settings
(setq make-backup-files t
      backup-by-copying t      ; don't clobber symlinks
      backup-by-copying-when-linked t
      backup-directory-alist '(("." . "~/.cache/emacs/backup"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;; Autosave
(setq auto-save-default nil)

;; (setq auto-save-file-name-transforms
;; `((".*" "~/.cache/emacs/saves/" t)))

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox-bin")

;; Allow commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Enable recursive minibuffers, i.e. you can use M-x inside M-x
(setq enable-recursive-minibuffers t)

;; TODO: add var
;; Auto-indent when pasting
;; (dolist (command '(yank yank-pop))
;;   (eval `(defadvice ,command (after indent-region activate)
;;            (and (not current-prefix-arg)
;;                 (let ((mark-even-if-inactive transient-mark-mode))
;;                   (indent-region (region-beginning) (region-end) nil))))))

;; Use zsh
;; (setq shell-file-name "zsh")
;; (setq shell-command-switch "-ic")

;; Make Emacs use the $PATH set up by the user's shell
;; (when (display-graphic-p)
;;        (setq exec-path
;;              (or (eval-when-compile
;;                    (when (require 'exec-path-from-shell nil t)
;;                      (setq exec-path-from-shell-check-startup-files nil
;;                            exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments))
;;                      (nconc exec-path-from-shell-variables '("GOPATH" "GOROOT" "PYTHONPATH"))
;;                      (exec-path-from-shell-initialize)
;;                      exec-path))
;;                  exec-path)))

;; (when (memq window-system '(mac ns x))
;;   (require 'exec-path-from-shell)
;;   (setq-default exec-path-from-shell-shell-name "/bin/zsh")
;;   (nconc exec-path-from-shell-variables '("GOPATH" "GOROOT" "PYTHONPATH"))
;;   (exec-path-from-shell-initialize))

;; Save bookmarks when kill emacs
(add-hook 'kill-emacs-hook 'bookmark-save)

;; (use-package exec-path-from-shell
;;   :disabled
;;   :defer  2
;;   :config
;;   (dolist (var '("GOPATH"  "NVM_BIN"))
;;     (add-to-list 'exec-path-from-shell-variables var))
;;   (exec-path-from-shell-initialize))

(provide 'snug-settings)
