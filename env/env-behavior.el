;; Electric indent
(electric-indent-mode -1)
(setq-default electric-indent-inhibit t)

;; Undotree
(use-package undo-tree
  :ensure t
  :config
  ;; Persistent undo-tree history across Emacs sessions
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.cache/emacs/undo")))
  (add-hook 'write-file-functions #'undo-tree-save-history-hook)
  (add-hook 'find-file-hook #'undo-tree-load-history-hook)
  (add-hook 'find-file-hook #'global-undo-tree-mode-check-buffers)

  (setq undo-tree-visualizer-timestamps t)
;; (setq undo-tree-visualizer-diff t))
  (global-undo-tree-mode 1))


;; Recent files
;; TODO: auto cleanup
;; see: https://gist.github.com/masutaka/1325654/955277113028eb7b968453a5b7802b74b51b393d
;; TODO: disable message in minibuffer after auto-save
(use-package recentf
  :init
  (run-at-time nil (* 5 60) 'recentf-save-list)
  ;; Recentf blacklist
  (setq recentf-exclude '("^/var/folders\\.*"
                          "COMMIT_EDITMSG\\'"
                          ".*-autoloads\\.el\\'"
                          "[/\\]\\.emacs.d/recentf"
                          "[/\\]\\.emacs.d/bookmarks"
                          "[/\\]\\.emacs.d/url"
                          "^/usr/share/emacs"
                          "[/\\]\\.emacs.d/elpa"))

  (recentf-mode 1)
  :config
  (setq recentf-auto-cleanup 2)
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 50))

(use-package subword
  :config
  (global-subword-mode))

(use-package uniquify
  :defer .1
  :config
  (setq uniquify-buffer-name-style 'forward))


(provide 'env-behavior)
