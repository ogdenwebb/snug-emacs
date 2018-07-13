;; Company completion settings.  -*- lexical-binding: t; -*-

;; Autocomplete
(use-package company
  :ensure t
  :commands (global-company-mode company-complete-common)
  :init (add-hook 'prog-mode-hook 'global-company-mode)
  :config
  ;; (add-hook 'after-init-hook 'global-company-mode)

  ;; Use fuzzy completion
  (use-package company-flx
    :after company
    :ensure t
    :config
    (company-flx-mode +1))

  ;; TODO:
  ;; dabbrev hides other normal condidats
  (add-to-list 'completion-styles 'initials t)

  (setq company-idle-delay nil ; never start completions automatically
        company-require-match nil
        company-minimum-prefix-length 3
        company-tooltip-align-annotations t)

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
    :commands (company-shell company-shell-env company-shell-rebuild-cache)
    :config
    (add-to-list 'company-backends '(company-shell company-shell-env)))

  ;; (use-package company-statistics
  ;;   :after company
  ;;   :config
  ;;   (setq company-statistics-file "~/.cache/emacs/company-statistics-cache.el")
  ;;   (company-statistics-mode +1))

  ;; Complete filename
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-files))


  ;; Tern
  (use-package company-tern
    :after (company tern)
    :commands (company-tern)
    :config
    (add-to-list 'company-backends 'company-tern))

  ;; Web-mode
  (use-package company-web-html
    :after (company web-mode)
    :config
    (add-to-list 'company-backends 'company-web-html))

  ;; Indent empty string and enable TAB completion
  (setq tab-always-indent 'complete)

  (defvar completion-at-point-functions-saved nil)

  (defun company-complete-common-wrapper ()
    (let ((completion-at-point-functions completion-at-point-functions-saved))
      (company-complete-common)))

  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
          (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))

  ;; (with-eval-after-load 'company
  ;;   (define-key company-mode-map [remap indent-for-tab-command]
  ;;     'company-indent-for-tab-command))


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
  (setq company-dabbrev-downcase nil))

  ;; (setq company-dabbrev-ignore-case nil)
  ;; (setq company-dabbrev-code-ignore-case nil)
  ;; (setq company-dabbrev-code-everywhere t)
  ;; (setq company-dabbrev-code-modes t)
  ;; (setq company-dabbrev-code-other-buffers t)
  ;; (setq company-dabbrev-ignore-buffers "\\`\\'")

(provide 'env-company)
