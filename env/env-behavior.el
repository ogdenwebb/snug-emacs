;;; Various packages -*- lexical-binding: t; -*-

;; Electric
(use-package electric
  :straight nil
  :hook ((after-init . electric-indent-mode))
         ;; (after-init . electric-layout-mode))
  :config
  ;; (add-to-list 'electric-layout-rules '(?{ . after-stay))
  (setq-default electric-indent-inhibit t))

(use-package undo-propose
  :straight (:host github :repo "jackkamm/undo-propose-el" :files ("*.el"))
  :commands (undo-propose undo-propose-diff)
  :config
  (undo-propose-wrap redo)
  (setq undo-propose-pop-to-buffer t))

;; Simple, stable linear undo with redo for Emacs.
(use-package undo-fu
  :commands (undo-fu-only-undo undo-fu-only-redo)
  :init
  (setq evil-undo-system 'undo-fu))

;; Save & recover undo steps between Emacs sessions.
(use-package undo-fu-session
  :hook (after-init . global-undo-fu-session-mode)
  :config
  (setq undo-fu-session-file-limit 50
        undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'"
                                             "/git-rebase-todo\\'")))

;; Visualize the undo tree
(use-package vundo
  :straight (:host github :repo "casouri/vundo")
  :general
  (general-define-key
   :keymaps 'vundo-mode-map
   "l"       'vundo-forward
   "<right>" 'vundo-forward
   "h"       'vundo-backward
   "<left>"  'vundo-backward
   "j"       'vundo-next
   "<down>"  'vundo-next
   "k"       'vundo-previous
   "<up>"    'vundo-previous
   "a"       'vundo-stem-root
   "e"       'vundo-stem-end
   "q"       'vundo-quit
   "C-g"     'vundo-quit
   "RET"     'vundo-confirm
   )
  )

;; If you want Emacs kill ring and system clipboard to be independent.
;; (use-package simpleclip
;;   :config
;;   (simpleclip-mode t))

(defvar snug-recentf-cleanup-symlinks t
  "When t, delete symbolic links from the list with recent opened files.")

;; Recent files
(use-package recentf
  :straight nil
  :defer 1
  :hook (kill-emacs . snug/recentf-cleanup-and-save)
  :config
  (defun snug/recentf-save-list-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if (fboundp 'shut-up)
          (shut-up (recentf-save-list))
        (recentf-save-list)))
    (message ""))

  (defun snug/recentf-cleanup-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if (fboundp 'shut-up)
          (shut-up (recentf-cleanup))
        (recentf-cleanup)))
    (message ""))

  (defun snug/recentf-cleanup-and-save ()
    (progn (snug/recentf-cleanup-silence)
           (snug/recentf-save-list-silence)))

  (run-at-time nil (* 5 60) 'snug/recentf-save-list-silence)

  (setq recentf-max-menu-items 150
        recentf-max-saved-items 150
        recentf-auto-cleanup 'never
        ;; Recentf blacklist
        recentf-exclude '("^/var/folders\\.*"
                          "COMMIT_EDITMSG\\'"
                          ".*-autoloads\\.el\\'"
                          "[/\\]\\.emacs.d/recentf"
                          "[/\\]\\.emacs.d/.cache"
                          "[/\\]\\.emacs.d/bookmarks"
                          "[/\\]\\.emacs.d/url"
                          "[/\\]\\.emacs.d/straight"
                          ".*.jpg$"
                          ".*.png$"
                          ".*.gif$"
                          ".cache"
                          "cache"
                          "^/usr/share/emacs"
                          "^/usr/lib64/go/src"
                          "[/\\]\\.emacs.d/elpa"))

  ;; Don't show files inside no-littering directory
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  ;; Delete symbolic links from recentf
  (when snug-recentf-cleanup-symlinks
    (add-to-list 'recentf-exclude (lambda (f) (not (string= (file-truename f) f)))))

  (recentf-mode t)
  )

(use-package subword
  ;; :commands (subword-mode global-subword-mode)
  :hook (after-init . global-subword-mode))

(use-package uniquify
  :straight nil
  :defer 2
  :config
  (setq uniquify-buffer-name-style 'forward))


(use-package unfill
  :commands (unfill-region unfill-paragraph unfill-toggle))

;; Make scripts executable automatically.
(use-package executable
  :ensure nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;; Better help
(use-package helpful
  :if (>= emacs-major-version 25)
  :defer .1
  :config)

;; Emacs generic Help
;; (use-package ghelp
;;   :straight (:host github :repo "casouri/ghelp")
;;   :config
;;   )

(use-package separedit
  :commands (separedit)
  :general
  (general-define-key :keymaps '(minibuffer-local-map prog-mode-map)
                      "C-x C-e" 'separedit))

(provide 'env-behavior)
