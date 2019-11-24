;; Mercurial  -*- lexical-binding: t -*-
;; (use-package monky
;;   :config
;;   (setq monky-process-type 'cmdserver))

(setq vc-annotate-background nil
      vc-annotate-very-old-color nil)

;; Git
(use-package magit
  :init
  ;; Disable GUI dialog for git
  (setenv "GIT_ASKPASS" "")
  (setenv "SSH_ASKPASS" "")
  :commands (magit-status magit-diff magit-blame magit-commit)
  :config
  ;; Disable arrows in section heading
  (setq magit-section-visibility-indicator nil
        magit-diff-buffer-file-locked nil))
  ;; (setq magit-diff-refine-hunk 'all))

;; TODO: support github in my workflow
;; (use-package forge
;;   :after magit)

(use-package gist
  :defer t
  :commands (gist-list))

;; Highlight changes in git
(use-package git-gutter
  :defer t
  :hook (after-init . global-git-gutter-mode))

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
