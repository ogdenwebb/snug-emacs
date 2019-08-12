;;; Snug settings file -*- lexical-binding: t -*-
;
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
  :ensure nil
  :hook (after-init . delete-selection-mode))

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
              suggest-key-bindings nil)

;; Avoid GUI dialogs
(setq use-dialog-box nil)

;; Do not use GTK tooltips
(setq x-gtk-use-system-tooltips nil)

;; Disable warning about large files
(setq large-file-warning-threshold nil)

(setq bidi-display-reordering nil)

;; Replacing yes/no with y/n.
(defalias 'yes-or-no-p 'y-or-n-p)

;; y/n instead of yes/no when quitting
(setq confirm-kill-emacs 'y-or-n-p)

(setq-default indicate-buffer-boundaries nil ;  Don't show where buffer starts/ends
              indicate-empty-lines nil
              sentence-end-double-space nil
              kill-whole-line t)	; Kill line including '\n'

;; Keep cursor at end of lines.
(setq track-eol t
      line-move-visual nil)	 ; To be required by track-eol


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

;; Clean whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Auto reload buffer if file was changed
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

;; Lines
(setq indicate-empty-lines t
      require-final-newline t)

;; Disable bell
(setq visible-bell nil
      ring-bell-function 'ignore)

;; Truncate lines
(setq-default truncate-lines t)

;; TODO:
;; (setq-default global-visual-line-mode t)

(setq find-file-visit-truename t)
;; VC settings
(setq vc-follow-symlinks t)
;; (remove-hook 'find-file-hooks 'vc-find-file-hook)
;; (setq vc-handled-backends nil)

;; Suppress ad-handle-definition warnings
(setq ad-redefinition-action 'accept)

;; Resize frame pixelwise
(setq frame-resize-pixelwise t)

;; Save last position
(use-package saveplace
  :ensure nil
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

;; Open file in dired with xdg-open
;; (use-package dired
;;   :config
;;   (defun dired-open-file ()
;;     "In dired, open the file named on this line."
;;     (interactive)
;;     (let* ((file (dired-get-filename nil t)))
;;       (message "Opening %s..." file)
;;       (call-process "xdg-open" nil 0 nil file)
;;       (message "Opening %s done" file))

;;     ;; select file or directory.
;;     (define-key dired-mode-map (kbd "RET") 'dired-open-file)))

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

;; Eldoc
;; Disable eldoc autostart, enabled by default since 25
(global-eldoc-mode -1)

;; Enable recursive minibuffers
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
