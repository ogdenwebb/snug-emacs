;;; env-session - manage sessions -*- lexical-binding: t; -*-
(use-package desktop
  :hook (after-init . desktop-save-mode)
  :config
  (setq
   ;; desktop-path              (concat no-littering-var-directory "sessions")
        ;; desktop-dirname           (concat no-littering-var-directory "sessions")
        desktop-base-file-name ".desktop"
        desktop-base-lock-name ".desktop.lock"
        desktop-restore-reuses-frames t
        desktop-restore-in-current-display t
        desktop-restore-forces-onscreen t
        ;; desktop-auto-save-timeout 600
        ;; desktop-restore-frames nil
        ;; desktop-load-locked-desktop
        desktop-save 'if-exists))

(defun session-save ()
  (interactive)
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))

(defun session-save ()
  (interactive)
  (desktop-revert))

(use-package session
  :hook (after-init . session-initialize)
  :init
  (setq session-save-file (no-littering-expand-var-file-name ".session")
        session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)"
        session-save-file-coding-system 'utf-8))
        ;; desktop-globals-to-save
        ;; (append '((comint-input-ring        . 50)
        ;;           (compile-history          . 30)
        ;;           desktop-missing-file-warning
        ;;           (dired-regexp-history     . 20)
        ;;           (extended-command-history . 30)
        ;;           (face-name-history        . 20)
        ;;           (file-name-history        . 100)
        ;;           (grep-find-history        . 30)
        ;;           (grep-history             . 30)
        ;;           (ivy-history              . 100)
        ;;           (magit-revision-history   . 50)
        ;;           (minibuffer-history       . 50)
        ;;           (org-clock-history        . 50)
        ;;           (org-refile-history       . 50)
        ;;           (org-tags-history         . 50)
        ;;           (query-replace-history    . 60)
        ;;           (read-expression-history  . 60)
        ;;           (regexp-history           . 60)
        ;;           (regexp-search-ring       . 20)
        ;;           register-alist
        ;;           (search-ring              . 20)
        ;;           (shell-command-history    . 50)
        ;;           tags-file-name
        ;;           tags-table-list
        ;;           kill-ring))))


(defun save-defaults ()
  (desktop-save desktop-dirname)
  (savehist-save)
  (bookmark-save))

(defun save-histories ()
  (let ((buf (current-buffer)))
    (save-excursion
      (dolist (b (buffer-list))
        (switch-to-buffer b)
        (save-history)))
    (switch-to-buffer buf)))

(defun save ()
  (interactive)
  (save-desktop)
  (save-defaults)
  (save-histories))

(provide 'env-sessions)

;;; env-session ends here
