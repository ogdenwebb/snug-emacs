;; Increase garbage collection for speedup
(setq-default gc-cons-threshold 20000000) ; or even 1000000000
              ;; gc-cons-percentage 0.2)

;; encoding
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8
      locale-coding-system 'utf-8)

;; Replace the region
(delete-selection-mode 1)

(setq yank-pop-change-selection t)

;; Enable system clipboard in terminal Emacs
;; See: https://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
(setq select-enable-clipboard t)

(unless window-system
  (when (getenv "DISPLAY")
    ;; Callback for when user cuts
    (defun xsel-cut-function (text &optional push)
      ;; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
    ;; Call back for when user pastes
    (defun xsel-paste-function()
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output)))
    ;; Attach callbacks to hooks
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)))

;; Replacing yes/no to y/n.
(defalias 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)

(setq-default indicate-buffer-boundaries nil)  ; don't show where buffer starts/ends

;; Indentation
(setq evil-shift-width 2)
(setq tab-width 2)
(setq tab-always-indent t)
(setq-default indent-tabs-mode nil)

;; (setq indent-line-function 'insert-tab)

(defun lisp-indent-setup ()
    ;; Set tab-width to 2
    (setq tab-width 2)
    ;; Set evil-shift-width to 2
    (setq evil-shift-width 2))

(add-hook 'emacs-lisp-mode-hook 'lisp-indent-setup)
(add-hook 'clojure-mode-hook 'lisp-indent-setup)

;; Clean whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Auto reload buffer if file was changed
(global-auto-revert-mode)

;; Lines
(setq indicate-empty-lines t
      require-final-newline t)

;; Disable bell
(setq ring-bell-function 'ignore)

;; VC settings
;; Built-in VC backend is fucking slow
(setq vc-follow-symlinks t)
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(setq vc-handled-backends nil)

;; Save last position
(save-place-mode 1)

;; Disable lockfiles
(setq create-lockfiles nil)

;; Save history
(use-package savehist
  :init (savehist-mode 1)
  :config
  (setq savehist-additional-variables
        '(search-ring
          kill-ring
          set-variable-value-history
          shell-command-history
          file-name-history
          regexp-search-ring))
  (setq history-length 1000))

;; Backup settings
(setq backup-by-copying t      ; don't clobber symlinks
  backup-directory-alist '(("." . "~/.cache/emacs/backup"))
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)       ; use versioned backups

;; Autosave
(setq auto-save-default nil) ; because of low load speed

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
          browse-url-generic-program "chromium")

(provide 'env-common)
