;;; snug-emacs core stuff -*- lexical-binding: t; -*-

;; Common modules
(defvar snug-env '(maps face plugins))

(defvar snug-uses nil
  "List of USE flags for snug-emacs.")

;; if USE exists; mb used
;; (defun usep)

; (defalias 'exep 'executable-find)

(provide 'snug-core)
