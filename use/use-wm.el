;; Window management  -*- lexical-binding: t; -*-

;; undo/redo changes to Emacs' window layout
(use-package winner
  :straight nil
  :hook (after-init . winner-mode)
  :config
  (defvar winner-dont-bind-my-keys t))

(use-package eyebrowse
  :hook (after-init . eyebrowse-mode)
  :config
  (setq eyebrowse-mode-line-separator "|"))
  ;; (setq eyebrowse-new-workspace "*dashboard*"))

(provide 'use-wm)
