;; Dashboard
;; TODO: add evil binding
(use-package dashboard
  :disabled t
  :config
  ;; (use-package dashboard-project-status
  ;;  :config
  ;;   (add-to-list 'dashboard-item-generators
  ;;                `(project-status . ,(dashboard-project-status "~/.snug")))
  ;;   (add-to-list 'dashboard-items '(project-status) t)
  ;;   (setq dashboard-items '((project-status . 10)
  ;;                           (recents        . 10))))
  ;;                           ;; (agenda         . 10))))

  (setq dashboard-center-content t)

  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        ; (projects . 5)
                        ; (agenda . 5)
                        (registers . 5)))

  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))


  (setq dashboard-set-navigator t)
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons
        `(;; line1
          ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "homepage")))
           ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
           ("?" "" "?/h" #'show-help nil "<" ">"))
          ;; line 2
          ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
            "Linkedin"
            ""
            (lambda (&rest _) (browse-url "homepage")))
           ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))))

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
