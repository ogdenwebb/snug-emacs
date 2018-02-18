;; Autocomplete
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)

  ;; Use fuzzy completion
  (use-package company-flx
    :after company
    :ensure t
    :config
    (company-flx-mode +1))

  ;; TODO:
  ;; dabbrev hides other normal condidats
  (add-to-list 'completion-styles 'initials t)

  (setq company-idle-delay nil) ; never start completions automatically
  (setq company-require-match nil)
  (setq company-minimum-prefix-length 3)

  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)
    (define-key company-active-map (kbd "RET") #'company-complete-selection)
    (define-key company-active-map [return]    #'company-complete-selection))

  ;; Additional backends and company related package
  (use-package company-shell
    :after (company sh-script)
    :config
    (add-to-list 'company-backends '(company-shell company-shell-env))))

  ;; OCaml
  ;;;; Make company aware of merlin
  ;; (with-eval-after-load 'company
  ;;   (add-to-list 'company-backends 'merlin-company-backend))
  ;; Enable company on merlin managed buffers
  ;; (add-hook 'merlin-mode-hook 'company-mode)
  ;; Or enable it globally:
  ;; (add-hook 'after-init-hook 'global-company-mode)

  ;; Complete filename
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-files))


  ;; Tern
  (use-package company-tern
    :after (company tern)
    :config
    (add-to-list 'company-backends 'company-tern))

  ;; Web-mode
  (use-package company-web-html
    :after (company web-mode)
    :config
    (add-to-list 'company-backends 'company-web-html))

  ;; Indent empty string and enable TAB completion
  (with-eval-after-load 'company
    (define-key company-mode-map [remap indent-for-tab-command]
      'company-indent-for-tab-command))

  (setq tab-always-indent 'complete)

  (defvar completion-at-point-functions-saved nil)

  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
          (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))

  (defun company-complete-common-wrapper ()
    (let ((completion-at-point-functions completion-at-point-functions-saved))
      ;; (company-complete-common)))
      (company-complete-common)))


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
