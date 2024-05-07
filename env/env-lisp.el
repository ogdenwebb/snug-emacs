;;; Lisp editing -*- lexical-binding: t; -*-
(use-package elisp-mode
  :ensure nil
  :hook (emacs-lisp-mode . eldoc-mode))

;; Improve readability of escape characters in ELisp regular expressions
(use-package easy-escape
  :hook ((lisp-mode emacs-lisp-mode) . easy-escape-minor-mode)
  :config
  (set-face-attribute 'easy-escape-face nil :foreground (face-foreground 'error))
  (setq easy-escape-character ?╲))

(setq test-regex "\\*\\(?:.\\|\n\\)*?\\*/\\|//\\(?:.\\|\n\\)*?$")

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

;;  Lispy is reimagines Paredit - a popular method to navigate and edit LISP code in Emacs.
;; TODO: re-enable maybe
(use-package lispy
  :disabled t
  :defer t
  :config
  (setq lispy-close-quotes-at-end-p t))

;; lispy + evil = lispyville
(use-package lispyville
  :disabled t
  :hook ((common-lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)
         (racket-mode . lispy-mode)
         (hy-mode . lispy-mode)
         (lfe-mode . lispy-mode)
         (clojure-mode . lispy-mode))
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

(provide 'env-lisp)
