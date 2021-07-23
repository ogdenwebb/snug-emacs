;;  -*- lexical-binding: t; -*-

(use-package diff
  :defer t
  :config
  (setq diff-font-lock-prettify t))

;; (use-package ediff
;;   :commands (ediff))

;; (use-package evil-ediff
;;   :if (featurep 'evil)
;;   :after ediff)

(provide 'use-diff)
