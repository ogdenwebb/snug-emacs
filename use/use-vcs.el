;; Mercurial
;; (use-package monky
;;   :config
;;   (setq monky-process-type 'cmdserver))

(use-package ediff
  :defer t
  :config
  (use-package evil-ediff))

;; FIXME: conflict with git-gutter
;; (defun nlinum-disable (&optional frame)
;;   (interactive)
;;   (global-nlinum-mode -1))

;; (defun nlinum-enable (&optional frame)
;;   (interactive)
;;   (global-nlinum-mode +1))

;; Line numbering
;; (use-package nlinum
;;   :config
;;   (add-hook 'before-make-frame-hook 'nlinum-disable)
;;   (add-hook 'after-make-frame-functions 'nlinum-enable)
;;   (setq nlinum-format " %4d ")

;;   (if (daemonp)
;;       (add-hook 'after-make-frame-functions 'nlinum-disable)
;;     (add-hook 'prog-mode-hook 'nlinum-mode)))

; Git
(use-package magit)

;; Highlight changes in git
(use-package git-gutter
  :init
  (add-hook 'prog-mode-hook 'git-gutter-mode)
  :config
  ;; FIXME: add reload
  ;; (add-hook 'post-command-hook 'git-gutter:update-all-windows)
  (add-hook 'focus-in-hook 'git-gutter:update-all-windows)

  (custom-set-variables
    ;; '(git-gutter:update-interval 2)
    '(git-gutter:window-width 1)
    ;; WARNING: "" contains tag space character to display line
    '(git-gutter:unchanged-sign "󠀠")
    '(git-gutter:added-sign "󠀠")
    '(git-gutter:modified-sign "󠀠")
    '(git-gutter:deleted-sign "󠀠"))
  :general
  (general-nvmap
   "] h" 'git-gutter:next-hunk
   "[ h" 'git-gutter:previous-hunk))

(provide 'use-vcs)
