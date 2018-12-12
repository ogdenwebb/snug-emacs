(use-package elisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(use-package litable
  :disabled t)

;; TODO:
(defun lisp-indent-setup ()
  ;; Set tab-width to 2
  (setq-local tab-width 2)
  ;; Set evil-shift-width to 2
  (setq-local evil-shift-width 2))

(add-hook 'lisp-mode-hook 'lisp-indent-setup)

;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (if (and (equal major-mode 'emacs-lisp-mode)
;;                      (file-exists-p (concat buffer-file-name "c")))
;;                 (delete-file (concat buffer-file-name "c")))))

(provide 'env-lisp)
