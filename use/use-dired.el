;;; Dired file browser -*- lexical-binding: t -*-

;; Open file in dired with xdg-open
(use-package dired
  :straight nil
  :config
  (defun dired-open-file ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process "xdg-open" nil 0 nil file)
      (message "Opening %s done" file))

    ;; select file or directory.
    (define-key dired-mode-map (kbd "RET") 'dired-open-file)))

;; (use-package dired+)
(use-package diredfl
  :defer t
  :hook (dired-mode . diredfl-mode))

(provide 'use-dired)
