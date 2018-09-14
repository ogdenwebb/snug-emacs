;; Mercurial
;; (use-package monky
;;   :config
;;   (setq monky-process-type 'cmdserver))

(setq vc-annotate-background nil
      vc-annotate-very-old-color nil)

(use-package ediff
  :defer t
  :commands (ediff)
  :config
  (use-package evil-ediff
    :after ediff))

; Git
(use-package magit
  :init
  (setenv "GIT_ASKPASS" "")
  (setenv "SSH_ASKPASS" "")
  :commands (magit-status magit-diff)
  :config)
  ;; (setq magit-diff-refine-hunk 'all))

;; TODO:
;; (use-package magithub
;;   :after magit
;;   :config (magithub-feature-autoinject t))

(use-package gist
  :commands (gist-list))

;; Highlight changes in git
(use-package git-gutter
  :config
  (global-git-gutter-mode t))

  ;; (custom-set-variables
  ;;  ;; WARNING: "" contains tag space character to display line
  ;;  '(git-gutter:unchanged-sign "󠀠")
  ;;  '(git-gutter:added-sign "󠀠")
  ;;  '(git-gutter:modified-sign "󠀠")
  ;;  '(git-gutter:deleted-sign "󠀠"))

  (use-package git-gutter-fringe
    :after git-gutter)
    ;; :config
    ;; (setq git-gutter-fr:side 'right-fringe))

(provide 'use-vcs)
