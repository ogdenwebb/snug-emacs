;;; Dired file browser -*- lexical-binding: t -*-

;; (use-package dired+)
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(provide 'use-dired)
