;; Autocomplete
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (use-package company-flx
         :ensure t
         :config
         (company-flx-mode +1))
  :config
  ;; TODO:
  ;; dabbrev hides other normal condidats
  (add-to-list 'completion-styles 'initials t)

  (setq company-idle-delay nil) ; never start completions automatically
  (setq company-require-match nil)

  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous))

  ;; Additional backends and company related package
  (use-package company-shell
    :after company
    :config
    (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell))))

  ;; OCaml
  ;;;; Make company aware of merlin
  ;; (with-eval-after-load 'company
  ;;   (add-to-list 'company-backends 'merlin-company-backend))
  ;; Enable company on merlin managed buffers
  ;; (add-hook 'merlin-mode-hook 'company-mode)
  ;; Or enable it globally:
  ;; (add-hook 'after-init-hook 'global-company-mode)

  ;; Complete filename
  (add-to-list 'company-backends 'company-files)

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
  ;; (defvar company-mode/enable-yas t
  ;;   "Enable yasnippet for all backends.")

  ;; (defun company-mode/backend-with-yas (backend)
  ;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
  ;;       backend
  ;;     (append (if (consp backend) backend (list backend))
  ;;             '(:with company-yasnippet))))

  ;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  ;; (set (make-local-variable 'company-backends)
  ;;    '((company-elisp :with company-dabbrev-code)))

  ;; company-daabrev
  ;; (add-to-list 'company-backends 'company-dabbrev)
  ;; (add-to-list 'company-backends 'company-dabbrev-code)
  ;; (add-to-list 'company-backends '(company-capf company-dabbrev-code))
  (setq company-dabbrev-downcase nil)
  ;; (setq company-dabbrev-ignore-case nil)
  ;; (setq company-dabbrev-code-ignore-case nil)
  ;; (setq company-dabbrev-code-everywhere t)
  ;; (setq company-dabbrev-code-modes t)
  ;; (setq company-dabbrev-code-other-buffers 'all)
  ;; (setq company-dabbrev-ignore-buffers "\\`\\'")


(provide 'env-company)
