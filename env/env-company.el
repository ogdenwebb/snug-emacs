;; Autocomplete
(use-package company
  :ensure t
  :config
  (setq company-idle-delay nil) ; never start completions automatically
  (setq company-require-match nil)

  ;;;; Make company aware of merlin
  ;; (with-eval-after-load 'company
  ;;   (add-to-list 'company-backends 'merlin-company-backend))
  ;; Enable company on merlin managed buffers
  ;; (add-hook 'merlin-mode-hook 'company-mode)
  ;; Or enable it globally:
  ;; (add-hook 'after-init-hook 'global-company-mode)

  ;; Complete filename
  (add-to-list 'company-backends 'company-files)

  ;; Add fuzzy matching
  (with-eval-after-load 'company
    (company-flx-mode +1))

  ;; Clojure
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)

  ;; Tern
  (use-package company-tern
    :config
    (add-to-list 'company-backends 'company-tern))

  ;; Web-mode
  (use-package company-web-html
    :config
    (add-to-list 'company-backends 'company-web-html))

  ;; weight by frequency
  ;; (setq company-transformers '(company-sort-by-occurrence))

  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  ;; company-daabrev
  ;; (add-to-list 'company-backends 'company-dabbrev)
  ;; (add-to-list 'company-backends 'company-dabbrev-code)
  ;; (setq company-ddabbrev-code-everywhere t)
  ;; (setq company-dabbrev-code-modes t)
  ;; (setq company-dabbrev-code-other-buffers 'all)
  ;; (setq company-dabbrev-ignore-buffers "\\`\\'")

  (add-hook 'after-init-hook 'global-company-mode)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))

(provide 'env-company)
