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
  (use-package evil-ediff))

; Git
(use-package magit
  :ensure t
  :init
  (setenv "GIT_ASKPASS" "")
  (setenv "SSH_ASKPASS" "")
  :commands (magit-status magit-diff))


;; TODO:
;; (use-package magithub
;;   :after magit
;;   :config (magithub-feature-autoinject t))

(use-package gist
  :commands (gist-list)
  :ensure t)

;; Highlight changes in git
(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode)
  (with-eval-after-load 'git-gutter
    (require 'git-gutter-fringe))

  (custom-set-variables
   ;; WARNING: "" contains tag space character to display line
   '(git-gutter:unchanged-sign "󠀠")
   '(git-gutter:added-sign "󠀠")
   '(git-gutter:modified-sign "󠀠")
   '(git-gutter:deleted-sign "󠀠"))


  :config
  (use-package git-gutter-fringe
    :after git-gutter
    :ensure t))
    ;; :config
    ;; (setq git-gutter-fr:side 'right-fringe))

(provide 'use-vcs)
