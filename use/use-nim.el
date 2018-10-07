(use-package nim-mode
  :mode (("\\.nim\\'" . nim-mode)
         ("\\.nims\\'" . nimscript-mode))
  :config
  ;; (add-hook 'nim-mode-hook (lambda () (electric-indent-mode -1)))
  (add-hook 'nim-mode-hook 'flycheck-mode)
  (add-hook 'nim-mode-hook 'nimsuggest-mode))

;; (add-hook 'nim-mode-hook 'nimsuggest-mode)

;; (add-hook 'nim-mode-hook
;;         '(lambda () (setq-local electric-indent-chars '(?\s)))))

(defun nimsuggest-delete-home-logfile ()
  "Delete nimsuggest log file in $HOME directory."
  (interactive)
  (let ((nimsuggest-file (concat (getenv "HOME") "/nimsuggest.log")))
    (when (file-exists-p nimsuggest-file)
      (delete-file nimsuggest-file))))

;; (add-hook 'nim-mode-hook 'nimsuggest-delete-home-logfile)
;; (add-hook 'nimscript-mode-hook 'nimsuggest-delete-home-logfile)

(add-hook 'nim-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'nimsuggest-delete-home-logfile nil 'make-it-local)))

(add-hook 'nimscript-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'nimsuggest-delete-home-logfile nil 'make-it-local)))

(provide 'use-nim)
