;; Use ivy for fuzzy matching

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)

  ;; Fix for n and N in Evil
  ;; https://github.com/abo-abo/swiper/issues/89#issuecomment-183662338
  (defun my-swiper-update-search-ring-forward (&rest args)
    (add-to-history 'regexp-search-ring (ivy--regex ivy-text))
    (setq isearch-forward t))

  (defun my-swiper-update-search-ring-backward (&rest args)
    (add-to-history 'regexp-search-ring (ivy--regex ivy-text))
    (setq isearch-forward nil))

  (advice-add 'ivy-next-line :after #'my-swiper-update-search-ring-forward)
  (advice-add 'ivy-previous-line :after #'my-swiper-update-search-ring-backward)

  :config
  ;; (setq ivy-re-builders-alist
  ;;       ;; '((t . regexp-quote)))
  ;;       '((t . ivy--regex-fuzzy)))
  ;;    ;; '((t . ivy--regex-plus)))
  ;; (setq ivy-initial-inputs-alist nil)

  ;; Add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers nil)

  ;; Do not show "./" and "../" in the `counsel-find-file' completion list
  (setq ivy-extra-directories nil) ; default value: ("../" "./")

  (use-package swiper
    :ensure t)

  (use-package counsel
    :ensure t
    :config
    (setq counsel-find-file-ignore-regexp (regexp-opt '(".jpg" ".png" ".jpeg")))
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package ivy-rich
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
  (setq ivy-rich-path-style 'abbrev))

;; Helm setup to test theme faces
;; (use-package helm
;;   :diminish helm-mode
;;   :bind (("M-x" . helm-M-x)
;;          ("C-x C-f" . helm-find-files)
;;          ("C-x b" . helm-buffers-list))
;;   :init
;;   (helm-mode 1)
;;   (helm-autoresize-mode 1)
;;   (setq helm-M-x-fuzzy-match t
;;         helm-buffers-fuzzy-matching t
;;         helm-display-header-line nil)
;;   :config
;;   ;; No idea why here find-file is set to nil (so it uses the native find-file
;;   ;; for Emacs. This makes stuff like (find-file (read-file-name ...)) work with
;;   ;; Helm again.
;;   (add-to-list 'helm-completing-read-handlers-alist '(find-file . helm-completing-read-symbols)))

(provide 'env-ivy)
