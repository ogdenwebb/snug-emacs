;; Haskell -*- lexical-binding: t -*-
(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (add-hook 'haskell-mode-hook (lambda () (setq-local electric-indent-inhibit t)))

  ;; TODO: two empty lines
  (defun haskell-indentation-advice ()
    (when (and (< 1 (line-number-at-pos))
               (save-excursion
                 (forward-line -1)
                 (string= "" (s-trim (buffer-substring (line-beginning-position) (line-end-position))))))
      (delete-region (line-beginning-position) (point))))

  (advice-add 'haskell-indentation-newline-and-indent
              :after 'haskell-indentation-advice)

  (setq haskell-process-suggest-remove-import-lines t  ; warnings for redundant imports etc
        haskell-process-auto-import-loaded-modules t
        haskell-process-show-overlays (not (or (featurep 'flymake) (featurep 'flycheck))))

  ;; (add-hook 'haskell-mode-hook
  ;;           (lambda ()
  ;;             (set (make-local-variable 'company-backends)
  ;;                  (append '((company-capf company-dabbrev-code))
  ;;                          company-backends))))

  (defun snug/haskell-repl (&optional arg)
    "Opens a Haskell REPL."
    (interactive "P")
    (if-let (window
             (display-buffer
              (haskell-session-interactive-buffer (haskell-session))))
        (window-buffer window)
      (error "Failed to display Haskell REPL")))

  (defun snug/haskell-evil-open-above ()
    "Opens a line above the current line."
    (interactive)
    (evil-digit-argument-or-evil-beginning-of-line)
    (haskell-indentation-newline-and-indent)
    (evil-previous-line)
    (haskell-indentation-indent-line)
    (evil-append-line nil))

  (defun snug/haskell-evil-open-below ()
    "Opens a line below the current line."
    (interactive)
    (evil-append-line nil)
    (haskell-indentation-newline-and-indent))
  )

(use-package flycheck-haskell
  :defer t
  :hook (haskell-mode . flycheck-haskell-setup))

(use-package dante
  ;; :after haskell-mode
  :hook (haskell-mode . dante-mode)
         ;; (haskell-mode . flymake-mode))
  :init
  ;; (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  )

(provide 'use-haskell)
