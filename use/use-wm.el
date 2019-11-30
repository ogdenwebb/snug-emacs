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
  :defer t
  :hook (after-init . shackle-mode)
  :config
  (setq shackle-default-alignment 'below
        ;; shackle-default-size 0.4
        shackle-rules '(
                        ;; (help-mode           :align right :select t)
                        ;; (helpful-mode        :align right :select t)
                        ;; (compilation-mode    :select t   :size 0.25)
                        ;; ("*compilation*"     :select nil :size 0.25)
                        ;; ("*ag search*"       :select nil :size 0.25)
                        ;; ("*Flycheck errors*" :select nil :size 0.25)
                        ;; ("*Warnings*"        :select nil :size 0.25)
                        ;; ("*Error*"           :select nil :size 0.25)
                        ;; ("*Org Links*"       :select nil :size 0.1)
                        ;; (magit-status-mode   :other t :size 0.5)
                        ;; (magit-log-mode      :same t)
                        ;; ;; (magit-commit-mode   :ignore t)
                        ;; (magit-diff-mode     :select nil :other t :size 0.5)
                        ;; (git-commit-mode     :same t)
                        ;; (vc-annotate-mode    :same t)

                        (compilation-mode :select t :align t :size 0.4)
                        ("\\`\\*Org\sSrc.*?\\*.*\\'" :regexp t :align right :size 100)
                        ("\\`\\*Org-Babel\sError\sOutput\\*.*\\'" :regexp t :align t :size 0.4)
                        ("*compilation*" :select t :align t :size 0.4)
                        ("*Async Shell Command*" :select t :align t :size 0.4)
                        ("*Shell Command Output*" :select t :align t :size 0.4)
                        ("\\`\\*e?shell.*\\'" :regexp t :select t :popup t :align t :size 0.4)
                        (ejc-result-mode :select t :popup t :align t :size 0.5)
                        (comint-mode :select t :align t :size 0.4)
                        (help-mode :select t :align t :size 0.4)
                        (helpful-mode :select t :align t :size 0.4)
                        (magit-status-mode :select t :align t :size 0.4)
                        (magit-log-mode :same t :inhibit-window-quit t)
                        (magit-refs-mode :select t :same t :align t :size 0.4)
                        (magit-diff-mode :select nil :align right :size 0.5)
                        (magit-revision-mode :select t :align right :size 0.5)
                        (flycheck-error-list-mode :select t :align right :size 0.3)
                        (inferior-python-mode :select t :popup t :align t :size 0.4))
                        ))

(provide 'use-wm)
