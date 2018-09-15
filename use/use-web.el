;; Enable web-mode
;; sudo npm install -g csslint
(use-package web-mode
  :mode (("\\.html\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook 'turn-off-smartparens-mode)

  ;; (add-hook 'web-mode-hook #'(lambda () (highlight-indent-guides-mode -1)))
  ;; (add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'web-mode)))

  ;; Install tidy html5
  ;; (flycheck-add-mode 'html-tidy 'web-mode)

  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        ;; web-mode-enable-auto-closing t
        ;; web-mode-enable-auto-pairing t
        web-mode-enable-auto-opening t
        web-mode-enable-comment-interpolation t
        web-mode-enable-css-colorization t
        web-mode-enable-auto-indentation t
        web-mode-imenu-regexp-list
        '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
          ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
          ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
          ;; angular imenu
          (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "="))))

;; Emmet
(use-package emmet-mode
  :hook ((sgml-mode-hook css-mode-hook) . emmet-mode) ;; enable Emmet's css abbreviation.
  ;; Maps
  :general
  (general-imap
    "k" (general-key-dispatch 'self-insert-command
          :name gl/evil-emmet
          :timeout 0.25
          "j" 'emmet-expand-line)))

;; Web-mode
(use-package company-web-html
  :after (company web-mode)
  :config
  (add-to-list 'company-backends '(company-web-html :with company-etags)))


(provide 'use-web)
