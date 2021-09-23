;; Better solution for incremental narrowing in Emacs.  -*- lexical-binding: t -*-

(use-package selectrum
  :hook (after-init . selectrum-mode)
  :config
  ;; (setf (alist-get "<escape>" selectrum-minibuffer-bindings) #'abort-recursive-edit)
  :general
  (general-def selectrum-minibuffer-map
    "<escape>" #'abort-recursive-edit)
  )

;; To make sorting and filtering more intelligent
(use-package selectrum-prescient
  :hook (selectrum-mode . selectrum-prescient-mode))

;; Consulting completing-read
(use-package consult
  :defer t
  :after selectrum)

(use-package consult-selectrum
  :straight nil
  :after consult)

;; Consult integration for projectile
(use-package consult-projectile
  :after (consult projectile)
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

;; Enrich existing commands with completion annotations
(use-package marginalia
  :after selectrum
  :config
   ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))

  (marginalia-mode))

;; Emacs finally learns how to ctrl+F.
(use-package ctrlf
  :defer 2
  :config
  (ctrlf-mode t)
  :general
  (general-def ctrlf-minibuffer-mode-map
    "C-n" #'ctrlf-forward-default
    "C-p" #'ctrlf-backward-default))


;; TODO provides ivy-occur alternative
;; (use-package embark)

;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :after (embark consult)
;;   :demand t ; only necessary if you have the hook below
;;   ;; if you want to have consult previews as you move around an
;;   ;; auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . embark-consult-preview-minor-mode))


;; Remap commands
(when (eq snug-default-completion-system 'selectrum)
  (use-package consult
    :straight nil
    :general
    ([remap recentf-open-files]            #'consult-recent-file)
    ([remap list-buffers]                  #'consult-buffer)
    ([remap switch-to-buffer]              #'consult-buffer)
    ([remap switch-to-buffer-other-window] #'consult-buffer-other-window)

    ([remap imenu]                         #'consult-imenu)
    ([remap load-theme]                    #'consult-theme)
    ([remap yank-pop]                      #'consult-yank-pop)
    ([remap bookmark-jump]                 #'consult-bookmark)
    )

  (use-package consult-projectile
    :straight nil
    :general
    ([remap projectile-find-file] #'consult-projectile))
  )

(provide 'env-selectrum)
