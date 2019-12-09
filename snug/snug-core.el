;;; snug-emacs core stuff -*- lexical-binding: t; -*-

;; Common modules
;; (defvar snug-env '(maps face plugins))

;; (defvar snug-useflags nil
;;   "List of USE flags for snug-emacs.")

;; (defun snug/usep (use)
;;     "Check if USE is enabled."
;;     (memq use snug-useflags))

;; (defalias 'usep 'snug/usep)

;; (defun snug/use-add (use)
;;   "Add new USE to `snug-useflags'."
;;   (add-to-list 'snug-useflags use))

;; ; (defalias 'exep 'executable-find)

;; Convert keyword to string without colon
(defun keyword-to-name-str (keyword)
  "Return KEYWORD symbol without initial colon as string
i.e. :keyword to \"keyword\"."
  (substring (symbol-name keyword) 1))

(defmacro lambda! (&rest body)
  "A shortcut for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

;; Detect system type
(defconst *IS-LINUX*   (eq system-type 'gnu/linux))
(defconst *IS-MAC*     (eq system-type 'darwin))
(defconst *IS-WINDOWS* (memq system-type '(cygwin windows-nt ms-dos)))
(defconst *IS-BSD*     (or *IS-MAC* (eq system-type 'berkeley-unix)))

(provide 'snug-core)

;;; snug-core.el ends here
