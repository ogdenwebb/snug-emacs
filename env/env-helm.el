;; Helm setup to test theme faces

(use-package helm
  :disabled t
  :init
  (helm-mode +1))

  ;; No idea why here find-file is set to nil (so it uses the native find-file
  ;; for Emacs. This makes stuff like (find-file (read-file-name ...)) work with
  ;; Helm again.
  ;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . helm-completing-read-symbols))


(provide 'env-helm)
