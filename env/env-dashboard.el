;; Dashboard
(use-package dashboard
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  (setq dashboard-center-content t
        dashboard-path-style 'truncate-middle)
        ;; dashboard-page-separator "\n\f\n")

  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        ; (agenda . 5)
                        ;; (registers . 5)
                        ))

  ;; (setq dashboard-display-icons-p t)
  ;; (setq dashboard-icon-type 'all-the-icons)

  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t)

        ;; dashboard-projects-backend 'projectile
        ;; dashboard-projects-switch-function #'counsel-projectile-switch-project-by-name)

        ;; dashboard-heading-icons '((recents   . "nf-oct-history")
        ;;                           (bookmarks . "nf-oct-bookmark")
        ;;                           (agenda    . "nf-oct-calendar")
        ;;                           (projects  . "nf-oct-briefcase")
        ;;                           (registers . "nf-oct-database")))

  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))

  (setq dashboard-set-navigator t)

  (setq dashboard-navigator-buttons
        `(;; Line 1
          ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "homepage")))
           ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
           ("?" "" "?/h" #'show-help nil "<" ">"))
          ;; Line 2
          ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
            "Linkedin"
            ""
            (lambda (&rest _) (browse-url "homepage")))
           ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))))

  ;; Set the title
  (setq dashboard-banner-logo-title "I know no Evil")
  ;; Set the banner
  (setq dashboard-startup-banner 'logo)

  ;; Dashboard footer
  (setq dashboard-footer-icon
        (all-the-icons-fileicon "emacs" :height 1.1 :v-adjust 0.0))

  (dashboard-setup-startup-hook))

(provide 'env-dashboard)
