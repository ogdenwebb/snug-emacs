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
  :hook (selectrum-mode . selectrum-prescient-mode)
  )

(use-package consult
  :after selectrum)

(use-package consult-selectrum
  :straight nil
  :after consult)

(use-package marginalia
  :after selectrum
  :config
   ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))

  (marginalia-mode))

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

(use-package ctrlf
  :defer .5
  :config
  (ctrlf-mode t))

(use-package consult-projectile
  :after (consult projectile)
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

(provide 'env-selectrum)
