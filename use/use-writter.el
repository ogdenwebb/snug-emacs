;; Simple Emacs minor mode for a nice writing environment.
(use-package olivetti
  :commands (olivetti-mode olivetti-shrink olivetti-expand olivetti-toggle-hide-mode-line)
  :config
  (setq-default olivetti-body-width 80))

(provide 'use-writter)
