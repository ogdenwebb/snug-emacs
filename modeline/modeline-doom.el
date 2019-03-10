;; Mode-line based on doom-modeline package -*- lexical-binding: t -*-
(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-bar-width 3
        ;; doom-modeline-icon t
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-minor-modes nil))
        ;; doom-modeline-major-mode-icon t))

(provide 'modeline-doom)
