;; Enable web-mode
;; sudo npm install -g csslint
(use-package web-mode
  :mode (("\\.html\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook 'turn-off-smartparens-mode)
  (add-hook 'web-mode-hook #'(lambda () (highlight-indent-guides-mode -1)))
  (add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'web-mode)))

  ;; Install tidy html5
  (flycheck-add-mode 'html-tidy 'web-mode)

  (setq-default
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-enable-auto-closing t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-indentation t))

;; Javascript
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-idle-timer-delay 1)

  (use-package tern
    :config
    (add-hook 'js2-mode-hook (lambda () (tern-mode t)))))

;; Emmet
(use-package emmet-mode
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  ;; Maps
  :general
  (general-imap
   "k" (general-key-dispatch 'self-insert-command
         :name gl/evil-emmet
         :timeout 0.25
         "j" 'emmet-expand-line)))

(provide 'use-web)
