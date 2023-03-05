;; Web setup -*- lexical-binding: t -*-
;; Enable web-mode
;; sudo npm install -g csslint
(use-package web-mode
  :mode "\\.html\\'"
  :mode "\\.crotmp\\'"
  :mode "\\.p?html?$"
  :mode "\\.\\(tpl\\|blade\\)\\(\\.php\\)?$"
  :mode "\\.erb$"
  :mode "\\.jsp$"
  :mode "\\.as[cp]x$"
  :mode "\\.mustache$"
  :mode "\\.tsx$"
  :mode "\\.go\\(tmpl\\|html\\)$"
  :mode "wp-content/themes/.+/.+\\.php$"
  :config
  (add-hook 'web-mode-hook 'turn-off-smartparens-mode)
  ;; For emmet-mode to switch between HTML and CSS
  (add-hook 'web-mode-before-auto-complete-hooks
            #'(lambda ()
               (let ((web-mode-cur-lang
                      (web-mode-language-at-pos)))
                 (if (string= web-mode-cur-lang "css")
                     (setq emmet-use-css-transform t)
                   (setq emmet-use-css-transform nil)))))

  ;; (add-hook 'web-mode-hook #'(lambda () (highlight-indent-guides-mode -1)))
  ;; (add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'web-mode)))

  ;; Install tidy html5
  ;; (flycheck-add-mode 'html-tidy 'web-mode)

  (setq web-mode-attr-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-indent-style 2
        web-mode-markup-indent-offset 2
        web-mode-sql-indent-offset 2

        web-mode-enable-auto-quoting nil

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

;; CSS mode
(use-package css-mode
  :elpaca nil
  :mode ("\\.scss\\'" . scss-mode)
  :config
  (setq css-indent-offset 2)

  (add-hook 'css-mode-hook #'(lambda ()
                               (electric-pair-mode 1)
                               (setq electric-pair-preserve-balance nil))))

;; Sass: Syntactically Awesome Style Sheets
(use-package sass-mode
  :mode ("\\.sass\\'" . sass-mode))

;; Extra plugins
;; Emmet
(use-package emmet-mode
  :hook ((sgml-mode css-mode) . emmet-mode) ;; enable Emmet's css abbreviation.
  ;; Maps
  :general
  (general-imap
    "k" (general-key-dispatch 'self-insert-command
          :name gl/evil-emmet
          :timeout 0.25
          "j" 'emmet-expand-line)))

(use-package auto-rename-tag
  :disabled t
  :hook (web-mode . auto-rename-tag-mode))

(provide 'use-web)
