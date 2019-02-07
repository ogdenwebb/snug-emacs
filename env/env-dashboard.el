;; Dashboard
;; TODO: add evil binding
(use-package dashboard
  :disabled
  :config
  (use-package dashboard-project-status
   :config
    (add-to-list 'dashboard-item-generators
                 `(project-status . ,(dashboard-project-status "~/.snug")))
    (add-to-list 'dashboard-items '(project-status) t)
    (setq dashboard-items '((project-status . 10)
                            (recents        . 10))))
                            ;; (agenda         . 10))))
  ;; Set the title
  (setq dashboard-banner-logo-title "I know no Evil")
  ;; Set the banner
  ;; (setq dashboard-startup-banner 'logo)

  ;; (setq dashboard-items
  ;;       '((recents   . 5)
  ;;         (bookmarks . 5)
  ;;         (project  . 5))))
  (dashboard-setup-startup-hook))

(provide 'env-dashboard)
