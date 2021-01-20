;; Use ivy -*- lexical-binding: t -*-

(defvar snug-fuzzy-ivy nil
  "Use fuzzy completion in ivy.")

(use-package ivy
  :hook (after-init . ivy-mode)
  :general
  (general-def ivy-mode-map
    [remap list-buffers]                  #'ivy-switch-buffer
    [remap switch-to-buffer]              #'ivy-switch-buffer
    [remap switch-to-buffer-other-window] #'ivy-switch-buffer-other-window
    [remap imenu-anywhere]                #'ivy-imenu-anywhere)
  :config

  ;; Try to fix Emacs hangs
  (setq ivy-dynamic-exhibit-delay-ms 20 ; or 250
        counsel-async-filter-update-time 500000)

  ;; Set default ivy matchers
  (let ((standard-search-fn
         (if (featurep 'prescient)
             #'ivy-prescient-non-fuzzy
           #'ivy--regex-plus))
        (alt-search-fn
         (if snug-fuzzy-ivy
             #'ivy--regex-fuzzy
           ;; Ignore order for non-fuzzy searches by default
           #'ivy--regex-ignore-order)))
    (setq ivy-re-builders-alist
          `((counsel-rg     . ,standard-search-fn)
            (swiper         . ,standard-search-fn)
            (swiper-isearch . ,standard-search-fn)
            (t . ,alt-search-fn))
          ivy-more-chars-alist
          '((counsel-rg . 1)
            (counsel-search . 2)
            (t . 3))))

  (setq ivy-preferred-re-builders
        '((ivy--regex-plus . "plus")
          (ivy--regex-ignore-order . "order")
          (ivy--regex-fuzzy . "fuzzy")
          (ivy-prescient-non-fuzzy . "non fuzzy")))

  (setq ivy-display-style 'fancy
        ivy-height 12
        ivy-fixed-height-minibuffer nil


        ;; ivy-re-builders-alist
        ;; '((counsel-M-x . ivy--regex-fuzzy)
        ;;   (ivy-switch-buffer . ivy--regex-fuzzy)
        ;;   (ivy-switch-buffer-other-window . ivy--regex-fuzzy)
        ;;   (counsel-rg . ivy--regex-or-literal)
        ;;   (counsel-ag . ivy--regex-or-literal)
        ;;   (t . ivy--regex-plus))

        ;; Disable initial regexp
        ivy-initial-inputs-alist nil
        ;; Add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
        ivy-use-virtual-buffers t
        ;; Do not show "./" and "../" in the `counsel-find-file' completion list
        ;; ivy-extra-directories nil ; default value: ("../" "./")`'
        ;; ivy-count-format "[%d/%d] "
        ivy-count-format "%d/%d "
        )

  ;; Integrate ivy in projectile and magit
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))
  ;; (with-eval-after-load 'magit
  ;;   (setq magit-completing-read-function 'ivy))
  )

(use-package ivy-hydra
  :requires (hydra)
  :after (ivy))

(use-package swiper
  :after ivy
  :commands (swiper))

(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :general
  (general-def counsel-mode-map
    [remap execute-extended-command]  #'counsel-M-x
    [remap find-library]  #'counsel-find-library
    [remap describe-bindings]   #'counsel-descbinds
    [remap describe-face]   #'counsel-describe-face
    [remap list-faces-display]  #'counsel-faces
    [remap imenu]  #'counsel-imenu
    [remap load-library]  #'counsel-load-library
    [remap load-theme]  #'counsel-load-theme
    [remap yank-pop]  #'counsel-yank-pop
    [remap info-lookup-symbol]  #'counsel-info-lookup-symbol
    [remap pop-to-mark-command]  #'counsel-mark-ring
    [remap bookmark-jump]  #'counsel-bookmark)
  :config
  (setq counsel-find-file-ignore-regexp (regexp-opt '(".jpg" ".png" ".jpeg")))
  ;; (setq counsel-grep-base-command "ag -S --nogroup --nocolor --nofilename --numbers '%s' %s")

  ;; (when (execute-))
        ;; counsel-git-cmd "rg --files"
        ;; counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
        ;; counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode t))

;; More friendly interface for ivy
(use-package ivy-rich
  ;; :disabled t
  :after ivy
  :config
  ;; (dolist (cmd
  ;;          '(ivy-switch-buffer
  ;;            ivy-switch-buffer-other-window
  ;;            counsel-projectile-switch-to-buffer))
  ;;   (ivy-set-display-transformer cmd #'ivy-rich-switch-buffer-transformer))
  (setq ivy-rich-display-transformers-list (plist-put
            ivy-rich-display-transformers-list 'counsel-helpful-keymap-describe
            '(:columns ((counsel-describe-variable-transformer (:width 40))
                  (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))))
  (ivy-rich-set-display-transformer)
  (setq ivy-rich-path-style 'abbrev)
  (ivy-rich-mode t))

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :config
  (setq all-the-icons-ivy-rich-color-icon nil
        all-the-icons-ivy-rich-icon-size 0.85)
  (all-the-icons-ivy-rich-mode))

(use-package ivy-xref
  :after ivy
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel-css
  :after ivy)

(use-package ivy-yasnippet
  :after (ivy yasnippet))

;; (use-package ivy-posframe
;;   ;; :after (ivy posframe)
;;   :defer t
;;   :hook (ivy-mode . ivy-posframe-mode)
;;   :config
;;   (setq ivy-posframe-display-functions-alist
;;       '((swiper          . ivy-posframe-display-at-frame-center)
;;         (complete-symbol . ivy-posframe-display-at-frame-center)
;;         (counsel-M-x     . ivy-posframe-display-at-frame-center)
;;         (t               . ivy-posframe-display)))
;;   )

(provide 'env-ivy)
