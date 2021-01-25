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
  :after consult

  )

(use-package marginalia
  :after selectrum
  :config
   ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))

  (marginalia-mode))

;; TODO
;; (use-package embark)
;; (use-package embark-consult)

(use-package ctrlf
  :defer .5
  :config
  (ctrlf-mode t))

(provide 'env-selectrum)
