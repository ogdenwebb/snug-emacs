;; Mercurial
;; (use-package monky
;;   :config
;;   (setq monky-process-type 'cmdserver))

;; (use-package ediff
;;   :defer t
;;   :config
;;   (use-package evil-ediff))

;; Git
(use-package magit)

;; Highlight changes in git
(use-package git-gutter
  :defer t
  :if (not (window-system))
  :init
  (add-hook 'prog-mode-hook 'git-gutter-mode)
  :config
  (setq git-gutter:window-width 1)

  (setq git-gutter:update-interval 2)

  ;; WARNING: "" contains tag space character
  (custom-set-variables
    '(git-gutter:unchanged-sign "󠀠")
    '(git-gutter:added-sign "󠀠")
    '(git-gutter:modified-sign "󠀠")
    '(git-gutter:deleted-sign "󠀠"))
  :general
  (general-nvmap
   "] h" 'git-gutter:next-hunk
   "[ h" 'git-gutter:previous-hunk))

;; FIXME: conflict with git-gutter
;; Line numbering
;; (use-package nlinum
  ;; :config
;;   (setq nlinum-format "%3d ")
;;   (add-hook 'prog-mode-hook 'nlinum-mode))


(provide 'use-vcs)
