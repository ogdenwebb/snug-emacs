;;; Dired file browser -*- lexical-binding: t -*-

;; Dired settings
(use-package dired
  :ensure nil
  :hook (dired-mode . hl-line-mode)
  :config
  ;; Dired settings
  (setq dired-hide-details-hide-symlink-targets nil
        dired-omit-verbose nil
        dired-listing-switches "-AFhlv --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies 'always
        ;; dired-recursive-deletes 'always
        )

  (defun dired-open-file ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process "xdg-open" nil 0 nil file)
      (message "Opening %s done" file))

    ;; select file or directory.
    (define-key dired-mode-map (kbd "RET") 'dired-open-file)))

(use-package find-dired
  :ensure nil
  :after dired
  :config
  (setq find-ls-option
        '("-ls" . "-AFhlv --group-directories-first")
        find-name-arg "-iname"))

;; Extra font lock rules for a more colourful dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Dired Git info
(use-package dired-git-info
             :elpaca nil
  :commands (dired-git-info-mode))

;; Display subtrees in dired view.
;; For example you can toggle subtree with TAB key
(use-package dired-subtree
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil
        dired-subtree-line-prefix "  "))

(use-package dired-async
  :ensure (:repo "https://github.com/jwiegley/emacs-async" :files ("dired-async.el"))
  :after (dired async)
  :hook (dired-mode . dired-async-mode))

(provide 'use-dired)
