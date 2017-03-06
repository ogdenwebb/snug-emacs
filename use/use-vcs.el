;; Mercurial
;; (use-package monky
;;   :config
;;   (setq monky-process-type 'cmdserver))

(use-package ediff
  :defer t
  :config
  (use-package evil-ediff))

;; FIXME: conflict with git-gutter
(defun nlinum-disable (&optional frame)
  (interactive)
  (global-nlinum-mode -1))

(defun nlinum-enable (&optional frame)
  (interactive)
  (global-nlinum-mode +1))

(use-package nlinum
    :init
    (setq nlinum-format "%4d ")
    (add-hook 'prog-mode-hook 'nlinum-mode)
    (add-hook 'text-mode-hook 'nlinum-mode))

; Git
(use-package magit)

;; Highlight changes in git
(use-package git-gutter
  :init
  (global-git-gutter-mode)
  :config
  (use-package git-gutter-fringe)
  :general
  (general-nvmap
   "] h" 'git-gutter:next-hunk
   "[ h" 'git-gutter:previous-hunk))

;; (use-package git-gutter
;;   :commands (global-git-gutter-mode git-gutter-mode)
;;   :init
;;   (progn
;;     (add-hook 'prog-mode-hook 'git-gutter-mode)
;;     ;; FIXME: break down nlinum V
;;     ;; (git-gutter:linum-setup)
;;   ;; :config
;;   ;; FIXME: add live reload
;;   ;; (add-hook 'post-command-hook 'git-gutter:update-all-windows)
;;   ;; (add-hook 'focus-in-hook 'git-gutter:update-all-windows)

;;     (setq
;;      git-gutter:handled-backends '(git hg bzr svn)
;;      git-gutter:update-interval 2
;;      git-gutter:window-width 1
;;      git-gutter:diff-option "-w"
;;      git-gutter:hide-gutter t
;;      git-gutter:ask-p nil
;;      git-gutter:verbosity 0)

;;     (custom-set-variables
;;     ;; WARNING: "" contains tag space character to display line
;;      '(git-gutter:unchanged-sign "󠀠")
;;      '(git-gutter:added-sign "󠀠")
;;      '(git-gutter:modified-sign "󠀠")
;;      '(git-gutter:deleted-sign "󠀠"))

;;     ;; Color lines
;;     (custom-set-faces
;;       `(git-gutter:added ((t (:background "#54b685"))))
;;       `(git-gutter:modified ((t (:background "#acb370"))))
;;       `(git-gutter:deleted ((t (:background "#d75f5f"))))))
;;   :general
;;   (general-nvmap
;;    "] h" 'git-gutter:next-hunk
;;    "[ h" 'git-gutter:previous-hunk))

(provide 'use-vcs)
