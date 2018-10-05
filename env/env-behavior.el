;;; Various packages -*- lexical-binding: t; -*-
(require 'map)

;; Electric indent
(electric-indent-mode t)
(setq-default electric-indent-inhibit t)

;; Undotree
(use-package undo-tree
  :config
  ;; Persistent undo-tree history across Emacs sessions
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.cache/emacs/undo")))
  (add-hook 'write-file-functions #'undo-tree-save-history-hook)
  (add-hook 'find-file-hook #'undo-tree-load-history-hook)
  (add-hook 'find-file-hook #'global-undo-tree-mode-check-buffers)

  (setq-default undo-tree-visualizer-timestamps t)
  (setq-default undo-tree-visualizer-diff t)
  (global-undo-tree-mode 1))


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
  ;; Recentf blacklist
  (setq recentf-exclude '("^/var/folders\\.*"
                          "COMMIT_EDITMSG\\'"
                          ".*-autoloads\\.el\\'"
                          "[/\\]\\.emacs.d/recentf"
                          "[/\\]\\.emacs.d/.cache"
                          "[/\\]\\.emacs.d/bookmarks"
                          "[/\\]\\.emacs.d/url"
                          "^/usr/share/emacs"
                          "[/\\]\\.emacs.d/elpa"))

  ;; TODO: Do it when open file
  ;; Exclude a real file if there's a symlink for that in recentf
  (defvar elmax/recentf-remote-regexp "\\/\\w*?:\\w*?@\\w*?:\\/")
  (defun elmax/recentf-file-has-symlink (file)
    "Test whether there's a symbolic parent of FILE in `recentf-list'."
    (cl-some (lambda (x)
               (and (not (string= (file-truename x) file))
                    (string= x file)))
             recentf-list))

  (defun elmax/recentf-keep-symlinks (file)
    "Exclude local FILE that has a symbolic parent in `recentf-list'."
    ;; Unless the file is local
    (unless (string-match elmax/recentf-remote-regexp file)
      (elmax/recentf-file-has-symlink file)))

  (add-to-list 'recentf-exclude #'elmax/recentf-keep-symlinks)

  ;; (setq recentf-auto-cleanup 2)
  (recentf-mode 1)
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 75))

(use-package subword
  :commands (subword-mode global-subword-mode)
  :hook (after-init . global-subword-mode))

(use-package uniquify
  :defer 2
  :config
  (setq uniquify-buffer-name-style 'forward))


(provide 'env-behavior)
