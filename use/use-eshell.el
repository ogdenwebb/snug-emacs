;; -*- lexical-binding: t -*-

(use-package shell-pop
  :straight (:host github :repo "kyagi/shell-pop-el")
  :config
  (setq shell-pop-window-size 30
        shell-pop-full-span t
        shell-pop-window-position "bottom"
        shell-pop-autocd-to-working-dir t
        shell-pop-restore-window-configuration t
        shell-pop-cleanup-buffer-at-process-exit t)
  )

;; Customize prompt
(with-eval-after-load 'eshell
  (setq eshell-bad-command-tolerance 1
        eshell-cmpl-autolist t
        eshell-cmpl-cycle-completions nil
        eshell-cmpl-cycle-cutoff-length 2
        eshell-cmpl-ignore-case t
        eshell-cp-overwrite-files nil
        eshell-default-target-is-dot t


   ;; eshell-prompt-function
   ;;      (lambda nil
   ;;        (concat
   ;;         (eshell/pwd)
   ;;         " ~ ❯ "))


        eshell-destroy-buffer-when-process-dies t
        eshell-hist-ignoredups t
        eshell-list-files-after-cd t
        eshell-review-quick-commands t
        eshell-save-history-on-exit t
        eshell-scroll-show-maximum-output nil
        ;; eshell-stringify nil
        ;; TODO: explore
        eshell-visual-options nil

        ;; kill eshell buffer after exit
        ))

(provide 'use-eshell)
