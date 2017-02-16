;; Mercurial
(use-package monky
  :config
  (setq monky-process-type 'cmdserver))

(use-package ediff
  :defer t
  :config
  (use-package evil-ediff))

;; Git
(use-package magit)

;; Show changes in fringe
(use-package git-gutter-fringe
  :if window-system)

(provide 'use-vcs)
