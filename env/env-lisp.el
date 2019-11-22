;;; Lisp editing -*- lexical-binding: t; -*-
(use-package elisp-mode
  :straight nil
  :hook (emacs-lisp-mode . eldoc-mode))

(use-package litable
  :disabled t)

;; Showing an Elisp demo
(use-package elisp-demos
  :defer 1
  :config
  (if (featurep 'helpful)
      (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
    (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))
  )

;; TODO:
(defun lisp-indent-setup ()
  ;; Set tab-width to 2
  (setq-local tab-width 2)
  ;; Set evil-shift-width to 2
  (setq-local evil-shift-width 2))

(add-hook 'lisp-mode-hook 'lisp-indent-setup)
(add-hook 'emacs-lisp-mode-hook 'lisp-indent-setup)


(use-package lispy
  :defer t
  :hook ((common-lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)
         (racket-mode . lispy-mode)
         (hy-mode . lispy-mode)
         (lfe-mode . lispy-mode)
         (clojure-mode . lispy-mode))
  :config
  (setq lispy-close-quotes-at-end-p t)
  (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode))

(use-package lispyville
  :defer t
  ;; :when (featurep! :editor evil)
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '((operators normal)
     c-w
     (prettify insert)
     (atom-movement normal visual)
     slurp/barf-lispy
     (wrap normal insert)
     additional
     additional-insert
     (additional-wrap normal insert)
     (escape insert))))

;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (if (and (equal major-mode 'emacs-lisp-mode)
;;                      (file-exists-p (concat buffer-file-name "c")))
;;                 (delete-file (concat buffer-file-name "c")))))

(provide 'env-lisp)
