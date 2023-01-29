;;; Smart editing -*- lexical-binding: t -*-

;; NOTE: puni for everything and Lispy[ville] for supported langauges

;; puni - Structured editing that supports many major modes out of the box.
;; Treat as Smartparens alternative
(use-package puni
  :defer t
  :hook ((prog-mode sgml-mode nxml-mode tex-mode eval-expression-minibuffer-setup) . puni-mode)
  :hook ((prog-mode sgml-mode nxml-mode tex-mode eval-expression-minibuffer-setup) . electric-pair-mode)
  :config)

;; An emacs minor mode to automatically add spacing around operators
;; (use-package electric-operator
;;   :hook (prog-mode . electric-operator-mode))

(provide 'env-editing)
