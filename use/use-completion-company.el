;; Company completion settings.  -*- lexical-binding: t; -*-

;; Autocomplete
(use-package company
  :defer 5
  :commands (company-mode global-company-mode company-complete-common company-indent-or-complete-common
                          company-manual-begin company-grab-line)
  :config

  ;; TODO:
  ;; dabbrev hides other normal condidats
  (add-to-list 'completion-styles 'initials t)

  (setq company-idle-delay nil ; never start completions automatically
        company-require-match nil
        ;; company-show-numbers t

        ;; If enabled, selecting item before first or after last wraps around.
        company-selection-wrap-around t
        company-minimum-prefix-length 3
        company-tooltip-align-annotations t)

  (eval-after-load 'company-etags
    '(progn (add-to-list 'company-etags-modes 'web-mode)))
  (setq company-etags-everywhere '(php-mode html-mode web-mode nxml-mode))

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
  ;;   (company-statistics-mode t))

  ;; Complete filename
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-files))

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
  (defvar company-mode-enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-backend-with-yas (backend)
    (if (or (not company-mode-enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  ;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  ;; (set (make-local-variable 'company-backends)
  ;;    '((company-elisp :with company-dabbrev-code)))

  ;; company-daabrev
  ;; (add-to-list 'company-backends 'company-dabbrev)
  ;; (add-to-list 'company-backends 'company-dabbrev-code)
  ;; (add-to-list 'company-backends '(company-capf company-dabbrev-code))
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  ;; (setq company-dabbrev-code-ignore-case nil)
  ;; (setq company-dabbrev-code-everywhere t)
  ;; (setq company-dabbrev-code-modes t)
  ;; (setq company-dabbrev-code-other-buffers t)
  ;; (setq company-dabbrev-ignore-buffers "\\`\\'")

  (global-company-mode t)

  ;; Support yas in commpany
  ;; Note: Must be the last to involve all backends
  (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

;; Use fuzzy completion
(use-package company-flx
  :hook ((company-mode global-company-mode) . company-flx-mode))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package company-elisp
  :straight nil
  :after company
  :config
  (push 'company-elisp company-backends))

(use-package company-quickhelp
  :hook ((company-mode global-company-mode) . company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay nil))

;; TODO: how to specify mode
(defun snug/company-local-backend (mode backends)
  "Add BACKENDS to a buffer-local version of `company-backends'."
  (make-local-variable 'company-backends)
  (dolist (name backends)
    (cl-pushnew name company-backends)))

(with-eval-after-load 'company
  (defun company-complete-common-or-cycle-backward ()
    "Complete common prefix or cycle backward."
    (interactive)
    (company-complete-common-or-cycle -1))

  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map [return] 'company-complete-selection)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil))

(use-package company-try-hard
  :defer t
  :after (company)
  :commands (company-try-hard))

(provide 'use-completion-company)
