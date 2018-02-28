;; Helm setup to test theme faces
(use-package helm
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list))
  :config
  (helm-autoresize-mode 1)
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-display-header-line nil)
  ;; No idea why here find-file is set to nil (so it uses the native find-file
  ;; for Emacs. This makes stuff like (find-file (read-file-name ...)) work with
  ;; Helm again.
  ;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . helm-completing-read-symbols))
  (helm-mode 1))


(provide 'env-helm)
