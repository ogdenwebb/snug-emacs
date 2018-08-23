;; On-the-fly evaluation/substitution of Emacs lisp code
(use-package elisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(use-package litable
  :disabled t)

;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (if (and (equal major-mode 'emacs-lisp-mode)
;;                      (file-exists-p (concat buffer-file-name "c")))
;;                 (delete-file (concat buffer-file-name "c")))))

(provide 'env-lisp)
