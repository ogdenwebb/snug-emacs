;; Helm is incremental completion and selection narrowing framework -*- lexical-binding: t -*-

(use-package helm
  ;; :if
  :config
  (helm-mode t))

  ;; No idea why here find-file is set to nil (so it uses the native find-file
  ;; for Emacs. This makes stuff like (find-file (read-file-name ...)) work with
  ;; Helm again.
  ;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . helm-completing-read-symbols))


(provide 'env-helm)
