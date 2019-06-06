;; Window management  -*- lexical-binding: t; -*-

;; undo/redo changes to Emacs' window layout
(use-package winner
  :disabled t
  :straight nil
  :hook (after-init . winner-mode)
  :config
  (defvar winner-dont-bind-my-keys t))

(use-package eyebrowse
  :disabled t
  :hook (after-init . eyebrowse-mode)
  :config
  (setq eyebrowse-mode-line-separator "|"))
  ;; (setq eyebrowse-new-workspace "*dashboard*"))

(use-package shackle
  :hook (after-init . shackle-mode)
  :config
  (setq shackle-default-alignment 'below
        ;; shackle-default-size 0.4
        shackle-rules '((help-mode           :align right :select t)
                        (helpful-mode        :align right :select t)
                        (compilation-mode    :select t   :size 0.25)
                        ("*compilation*"     :select nil :size 0.25)
                        ("*ag search*"       :select nil :size 0.25)
                        ("*Flycheck errors*" :select nil :size 0.25)
                        ("*Warnings*"        :select nil :size 0.25)
                        ("*Error*"           :select nil :size 0.25)
                        ("*Org Links*"       :select nil :size 0.1)
                        (magit-status-mode   :other t :size 0.5)
                        (magit-log-mode      :same t)
                        ;; (magit-commit-mode   :ignore t)
                        (magit-diff-mode     :select nil :other t :size 0.5)
                        (git-commit-mode     :same t)
                        (vc-annotate-mode    :same t)
                        )))

(provide 'use-wm)
