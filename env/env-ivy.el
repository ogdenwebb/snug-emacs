;; Use ivy for fuzzy matching
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist
        ;; '((t . ivy--regex-fuzzy)))
        '((t . ivy--regex-plus)))
  ;; (setq ivy-initial-inputs-alist nil)

  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers nil)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

;; (use-package helm
;;   :ensure t
;;   :diminish helm-mode
;;   :bind (("M-x" . helm-M-x)
;;          ("C-x C-f" . helm-find-files)
;;          ("C-x b" . helm-buffers-list))
;;   :init
;;   (setq helm-M-x-fuzzy-match t
;;         helm-buffers-fuzzy-matching t
;;         helm-display-header-line nil)
;;   :config
;;   ;; No idea why here find-file is set to nil (so it uses the native find-file
;;   ;; for Emacs. This makes stuff like (find-file (read-file-name ...)) work with
;;   ;; Helm again.
;;   (helm-mode 1)
;;   (helm-autoresize-mode 1)
;;   (add-to-list 'helm-completing-read-handlers-alist '(find-file . helm-completing-read-symbols)))

(provide 'env-ivy)
