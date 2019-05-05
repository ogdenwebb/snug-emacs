;;; Various packages -*- lexical-binding: t; -*-

;; Electric indent
(electric-indent-mode t)
(setq-default electric-indent-inhibit t)

;; Undotree
(use-package undo-tree
  :disabled t)
  ;; :config
  ;; ;; Persistent undo-tree history across Emacs sessions
  ;; (setq undo-tree-auto-save-history t
  ;;       undo-tree-history-directory-alist '(("." . "~/.cache/emacs/undo"))
  ;;       undo-tree-enable-undo-in-region nil)
  ;; ;; TODO:
  ;; ;; (add-hook 'after-save-hook #'undo-tree-save-history)
  ;; ;; (add-hook 'find-file-hook #'undo-tree-load-history)
  ;; ;; (add-hook 'find-file-hook #'global-undo-tree-mode-check-buffers)
  ;; ;; (add-hook 'undo-tree-visualizer-mode-hook #'undo-tree-visualizer-toggle-diff)

  ;; (setq-default undo-tree-visualizer-timestamps t)
  ;; (setq-default undo-tree-visualizer-diff nil)
  ;; (global-undo-tree-mode 1))

(use-package undo-propose
  :straight (:host github :repo "jackkamm/undo-propose-el" :files ("*.el"))
  :commands (undo-propose undo-propose-diff)
  :config
  (undo-propose-wrap redo))

(use-package redo+
  :disabled t
  :straight (:host github :repo "clemera/undo-redo" :files ("*.el"))
  )

;; If you want Emacs kill ring and system clipboard to be independent.
;; (use-package simpleclip
;;   :config
;;   (simpleclip-mode t))

;; Recent files
;; TODO: auto cleanup
;; see: https://gist.github.com/masutaka/1325654/955277113028eb7b968453a5b7802b74b51b393d
;; TODO: disable message in minibuffer after auto-save
(use-package recentf
  ;; :no-require t
  :defer 1
  :config
  (defun recentf-save-list-silently ()
    (let ((inhibit-message t))
      (recentf-save-list)))
  (run-at-time nil (* 5 60) 'recentf-save-list-silently)

  (setq recentf-max-menu-items 100
        recentf-max-saved-items 100)
  ;; Recentf blacklist
  (setq recentf-exclude '("^/var/folders\\.*"
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
                          "^/usr/share/emacs"
                          "^/usr/lib64/go/src"
                          "[/\\]\\.emacs.d/elpa"))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  ;; Delete symbolic links from recentf
  (add-to-list 'recentf-exclude (lambda (f) (not (string= (file-truename f) f))))

  ;; (setq recentf-auto-cleanup 2)
  (recentf-mode 1))

(use-package subword
  :commands (subword-mode global-subword-mode)
  :hook (after-init . global-subword-mode))

(use-package uniquify
  :straight nil
  :defer 2
  :config
  (setq uniquify-buffer-name-style 'forward))


(provide 'env-behavior)
