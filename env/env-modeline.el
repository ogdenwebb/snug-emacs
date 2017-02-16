;; ;; Hide modes via rich minority
;; (use-package rich-minority
;;   :ensure t
;;   :config
;;   (setq rm-blacklist
;;         '(
;;           " yas"        ;; yasnippet
;;           " ctagsU"      ;; ctags update
;;           " Undo-Tree"      ;; undo tree
;;           " wr"        ;; Wrap Region
;;           " SliNav"      ;; elisp-slime-nav
;;           " Fly"        ;; Flycheck
;;           " GG"        ;; ggtags
;;           " ElDoc"      ;; eldoc
;;           " hl-highlight"
;;           " VHl"
;;           " ivy"
;;           " es"
;;           " company"
;;           " Parinfer:Indent"
;;           " Parinfer:Paren"
;;           " SP"
;;           " s-/")))

;; Telephone line
(use-package telephone-line
  :config
  ;; Need to create custom segments
  (use-package telephone-line-utils)

  ;; Set default separators: choose either of them
  (setq telephone-line-primary-left-separator 'telephone-line-identity-left)
  (setq telephone-line-primary-right-separator 'telephone-line-identity-right)
  ;; OR
  ;; (setq telephone-line-primary-left-separator 'telephone-line-cubed-left)
  ;; (setq telephone-line-primary-right-separator 'telephone-line-cubed-right)

  ;; Set subseparator
  (if window-system
      (progn
        (setq telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left)
        (setq telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right)))

  ;;;; Custom segments

  ;; Display major mode
  (telephone-line-defsegment* my-major-mode-segment
    (let ((mode (cond
                 ((string= mode-name "Emacs-Lisp") "Elisp")
                 ((string= mode-name "Javascript-IDE") "Javascript")
                 (t mode-name))))
      (propertize mode 'face `(:foreground "#835d83"))))

  ;; Display evil state
  (telephone-line-defsegment* my-evil-segment
    (if (telephone-line-selected-window-active)
      (let ((tag (cond
                  ((string= evil-state "normal") ":")
                  ((string= evil-state "insert") ">")
                  ((string= evil-state "replace") "r")
                  ((string= evil-state "visual") "v")
                  ((string= evil-state "operator") "=")
                  ((string= evil-state "motion") "m")
                  ((string= evil-state "emacs") "Emacs")
                  ((string= evil-state "multiedit") "Multi")
                  (t "-"))))
        tag)))

  ;; Display buffer name
  (telephone-line-defsegment* my-buffer-segment
    `(""
      ,(telephone-line-raw mode-line-buffer-identification t)))


  ;; Display current position in a buffer
  (telephone-line-defsegment* my-position-segment
    (if (telephone-line-selected-window-active)
        (if (eq major-mode 'paradox-menu-mode)
            (telephone-line-trim (format-mode-line mode-line-front-space))
          '(" %3l,%2c "))))

  ;; Display modified status
  (telephone-line-defsegment* my-modified-status-segment
    (if (buffer-modified-p)
        (propertize "+" 'face `(:foreground "#85b654"))
      ""))

  ;; Example of color string segment
  ;; (telephone-line-defsegment* my-color-segment
  ;;   (propertize "some-string" 'face `(:foreground "green")))

  ;; Display encoding system
  (telephone-line-defsegment* my-coding-segment
    (if (telephone-line-selected-window-active)
        (let* ((code (symbol-name buffer-file-coding-system))
               (eol-type (coding-system-eol-type buffer-file-coding-system))
               (eol (cond
                     ((eq 0 eol-type) "unix")
                     ((eq 1 eol-type) "dos")
                     ((eq 2 eol-type) "mac")
                     (t ""))))
          (concat eol " "))))

  ;; Left edge
  (setq telephone-line-lhs
        '((accent . "  ")
          (evil   . (my-evil-segment))
          (nil    . (my-buffer-segment))
          (nil    . (my-modified-status-segment))))

  ;; Right edge
  (setq telephone-line-rhs
        '((nil     . (telephone-line-misc-info-segment))
          (accent  . (my-position-segment))
          (nil     . (my-major-mode-segment))
          (accent  . (my-coding-segment))))
  ;; (accent . (my-position-segment))))

  (telephone-line-mode 1))

(provide 'env-modeline)
