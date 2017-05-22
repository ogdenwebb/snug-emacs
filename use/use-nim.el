(use-package nim-mode
  :config
  ;; (setq nim-nimsuggest-path "path/to/nimsuggest")
  ;; Currently nimsuggest doesn't support nimscript files, so only nim-mode...
  ;; (add-hook 'nim-mode-hook
            ;; '(lambda () (setq-local electric-indent-chars '(?\s))))

  ;; (add-hook 'nim-mode-hook 'nimsuggest-mode))
  (nimsuggest-mode -1)
  (flycheck-mode -1))

  ;; (add-to-list 'company-backends
  ;;              '(company-nim :with company-nim-builtin)))

(provide 'use-nim)
