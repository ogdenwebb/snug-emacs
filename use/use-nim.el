(use-package nim-mode
  :config
  ;; (setq nim-nimsuggest-path "path/to/nimsuggest")
  ;; Currently nimsuggest doesn't support nimscript files, so only nim-mode...
  (add-hook 'nim-mode-hook 'nimsuggest-mode))
  ;; (add-to-list 'company-backends
  ;;              '(company-nim :with company-nim-builtin)))

(provide 'use-nim)
