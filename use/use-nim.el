;; Nim programming language.  -*- lexical-binding: t; -*-
(use-package nim-mode
  :mode (("\\.nim\\'" . nim-mode)
         ("\\.nims\\'" . nimscript-mode))
  :hook ((nim-mode . nimsuggest-mode)
         (nim-mode . eldoc-mode)
         ;; (nim-mode . snug/company-disable-fuzzy)
         (nim-mode . snug--delete-nimlog-file)
         (nimscript-mode . snug--delete-nimlog-file))

  :config
  ;; TODO: "fix" for deferred error : (void-function incf)
  ;; (defalias 'incf 'cl-incf)

  (defun nimsuggest--delete-home-logfile ()
    "Delete nimsuggest log file in $HOME directory."
    (interactive)
    (let ((nimsuggest-file (concat (getenv "HOME") "/nimsuggest.log")))
      (when (file-exists-p nimsuggest-file)
        (delete-file nimsuggest-file))))

  (defun snug--delete-nimlog-file ()
    (add-hook 'after-save-hook 'nimsuggest--delete-home-logfile nil 'local))

  (add-hook 'nim-mode-hook (lambda () (electric-indent-mode -1)))
  :general
  (general-define-key :keymaps 'nim-mode-map
                      :states  '(normal)
                      "K"  'nimsuggest-show-doc))

;; (use-package flycheck-nim
;;   :disabled t
;;   :after (nim-mode flycheck)
;;   :hook (nim-mode . flycheck-mode))

;; (add-hook 'nim-mode-hook
;;         '(lambda () (setq-local electric-indent-chars '(?\s)))))

(provide 'use-nim)
