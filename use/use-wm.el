;; Window management  -*- lexical-binding: t; -*-

;; Undo/redo changes to Emacs' window layout
(use-package winner
  :ensure nil
  :hook (elpaca-after-init . winner-mode)
  :config
  (setq winner-dont-bind-my-keys t))

;; Quickly switch windows in Emacs
(use-package ace-window
  :defer t
  :init
  (global-set-key [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; (setq aw-dispatch-alist
  ;;       '((?s aw-swap-window "Swap Windows")
  ;;         (?2 aw-split-window-vert "Split Window Vertically")
  ;;         (?3 aw-split-window-horz "Split Window Horizontally")
  ;;         (?? aw-show-dispatch-help)))

  (setq aw-scope 'frame
        aw-display-mode-overlay t
        aw-background t))

;; Enforce rules for popup windows
(use-package shackle
  :hook (elpaca-after-init . shackle-mode)
  :config
  (defun snug/shackle--smart-split-dir ()
    (if (>= (window-pixel-height)
            (window-pixel-width))
        'below
      'right))

  ;; Smart window placement depending on current layout
  (defun snug/shackle-dynamic-tyling (buffer alist plist)
    (let
        ((frame (shackle--splittable-frame))
         (window (if (eq (snug/shackle--smart-split-dir) 'below)
                     (split-window-below)
                   (split-window-right))))
      (prog1
          (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
        (when window
          (setq shackle-last-window window
                shackle-last-buffer buffer))
        (unless (cdr (assq 'inhibit-switch-frame alist))
          (window--maybe-raise-frame frame)))))

  (setq shackle-default-alignment 'below
        ;; shackle-default-size 0.4
        shackle-rules '(
                        ;; (help-mode           :align right :select t)
                        ;; (helpful-mode        :align right :select t)
                        ;; (compilation-mode    :select t   :size 0.25)
                        ;; ("*compilation*"     :select nil :size 0.25)
                        ;; ("*ag search*"       :select nil :size 0.25)
                        ;; ("*Warnings*"        :select nil :size 0.25)
                        ;; ("*Error*"           :select nil :size 0.25)
                        ;; ("*Org Links*"       :select nil :size 0.1)
                        ;; (magit-status-mode   :other t :size 0.5)
                        ;; (magit-log-mode      :same t)
                        ;; ;; (magit-commit-mode   :ignore t)
                        ;; (magit-diff-mode     :select nil :other t :size 0.5)
                        ;; (git-commit-mode     :same t)
                        ;; (vc-annotate-mode    :same t)

                        ("\\*shell\\*" :regexp t :same t) ;; needed for shell-pop
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
                        (haskell-interactive-mode :select t :align t :size 0.4)
                        (magit-status-mode :select t :align t :size 0.4)
                        (magit-log-mode :same t :inhibit-window-quit t)
                        (magit-refs-mode :select t :same t :align t :size 0.4)
                        (magit-diff-mode :select nil :align right :size 0.5)
                        (magit-revision-mode :select t :align right :size 0.5)
                        ("*Flycheck errors*" :custom snug/shackle-dynamic-tyling :size 0.3)
                        (inferior-python-mode :select t :popup t :align t :size 0.4))
                        ))

(use-package eyebrowse
  :disabled t
  :hook (elpaca-after-init . eyebrowse-mode)
  :config
  (setq eyebrowse-mode-line-separator "|"))
  ;; (setq eyebrowse-new-workspace "*dashboard*"))

(use-package popper
  :hook (elpaca-after-init . popper-mode)
  ;; :bind (:map popper-mode-map
  ;;        ("C-`"   . popper-toggle-latest)
  ;;        ("M-`"   . popper-cycle)
  ;;        ("C-M-`" . popper-toggle-type))
  :config
  ;; (setq popper-group-function #'popper-group-by-directory)
  (setq popper-group-function #'popper-group-by-directory
        popper-window-height 30)

  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "vterm .+\\*$"
          help-mode
          helpful-mode
          compilation-mode))

  ;; Use shackle to control popup placement
  (setq popper-display-control nil)

  (with-eval-after-load 'telephone-line
    (setq popper-mode-line
          '(:eval (let ((face (if (telephone-line-selected-window-active)
                                  'telephone-line-accent-active
                                'telephone-line-accent-inactive)))
                    ;; (if (and (icons-displayable-p)
                    ;;          (bound-and-true-p telephone-line-mode))
                    ;;     (format " %s "
                    ;;             (nerd-icons-octicon "nf-oct-pin" :face face))
                      (propertize " POP" 'face face)))))


  ;; NOTE: To open vterm as a popup we can use `popper-lower-to-popup'
  )

(provide 'use-wm)
