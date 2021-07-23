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

;; (defmacro quiet! (&rest forms)
;;   "Run FORMS without making any noise."
;;   `(if init-file-debug
;;        (progn ,@forms)
;;      (let ((message-log-max nil))
;;        (with-temp-message (or (current-message) "") ,@forms))))

;; (defun quiet-function-advice (orig-fn &rest args)
;;   "Advice used to make a function quiet.
;; Call ORIG-FN with ARGS and suppress the output.

;; Example:
;;   (advice-add 'orig-fn :around #'quiet-function-advice)"
;;   (quiet! (apply orig-fn args)))

(defun snug/upgrade ()
  "Upgrade all installed packages using straight-x."
  (interactive)
  (progn
    (straight-x-fetch-all)
    (straight-rebuild-all)))

;; Detect system type
(defconst *IS-LINUX*   (eq system-type 'gnu/linux))
(defconst *IS-MAC*     (eq system-type 'darwin))
(defconst *IS-WINDOWS* (memq system-type '(cygwin windows-nt ms-dos)))
(defconst *IS-BSD*     (or *IS-MAC* (eq system-type 'berkeley-unix)))

(provide 'snug-core)

;;; snug-core.el ends here
