;; Dashboard
;; TODO: fix slow startup mb move it
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  ;; Set the title
  (setq dashboard-banner-logo-title "I know no Evil")
  ;; Set the banner
  (setq dashboard-startup-banner 'logo)

  (setq dashboard-items
        '((recents   . 5)
          (bookmarks . 5)
          (projects  . 5))))

(provide 'env-dashboard)
