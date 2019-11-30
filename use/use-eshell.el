;; -*- lexical-binding: t -*-
;; Customize prompt
(with-eval-after-load 'eshell
  (setq eshell-prompt-function
        (lambda nil
          (concat
           (eshell/pwd)
           " ~ ❯ "))
        eshell-bad-command-tolerance 1
        eshell-cmpl-autolist t
        eshell-cmpl-cycle-completions nil
        eshell-cmpl-cycle-cutoff-length 2
        eshell-cmpl-ignore-case t
        eshell-cp-overwrite-files nil
        eshell-default-target-is-dot t

        eshell-destroy-buffer-when-process-dies t
        eshell-hist-ignoredups t
        eshell-list-files-after-cd t
        eshell-review-quick-commands t
        eshell-save-history-on-exit t
        eshell-scroll-show-maximum-output nil
        ;; eshell-stringify nil
        ;; TODO: explore
        eshell-visual-options nil

        ))

(provide 'use-eshell)
