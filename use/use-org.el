;; Org-mode

(use-package org
  :config
  (setq org-startup-indented t)
  ;; Fontify
  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-ellipsis " ~ ")

  (defun org-init-hook ()
    (interactive)
    (nlinum-mode -1)
    ;; TODO: disable git-gutter
    ;; Enable line wrapping
    (visual-line-mode t))

  (add-hook 'org-mode-hook 'org-init-hook)

  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?D)
  (setq org-default-priority ?B)

  (custom-set-faces
   ;; '(org-todo ((t (:foreground "#5485b6" :bold t))))
   ;; OLD
   ;; '(org-todo ((t (:foreground "#5077a5" :bold t))))
   ;; #4e7f95
   ;; '(org-todo ((t (:foreground "#3B6FA3" :bold t))))

   '(org-done ((t (:foreground "dimgray" :bold t :strike-through t))))
   '(org-headline-done ((t (:foreground "dimgray" :bold nil :strike-through t)))))

  ;; Open links
  (setq org-link-frame-setup '((vm . vm-visit-folder)
                               (gnus . org-gnus-no-new-news)
                               (file . find-file)))
  (setq org-src-window-setup 'current-window)

  (use-package evil-org
    :after org
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              (lambda ()
                ;; (evil-org-set-key-theme)
                (evil-org-set-key-theme '(navigation insert textobjects)))))

  :general
  (general-define-key :keymaps 'org-mode-map
                      :states '(normal)
                      "RET" 'org-open-at-point
                      "gx"  'org-open-at-point
                      "t"   'org-todo
                      "za"  'org-cycle
                      "zA"  'org-shifttab
                      "zm"  'outline-hide-body
                      "zr"  'outline-show-all
                      "zo"  'outline-show-subtree
                      "zo"  'outline-show-all
                      "zc"  'outline-hide-subtree
                      ;; TODO: (??) fix outline-hide-all
                      "zc"  'outline-hide-sublevels
                      "T"   'org-insert-todo-heading-respect-content)
                      ;; "M-h"  'evil-window-left
                      ;; "M-j"  'evil-window-down
                      ;; "M-k"  'evil-window-up
                      ;; "M-l"  'evil-window-right)

  (general-define-key :keymaps 'org-mode-map
                      :prefix leader
                      :states '(normal)
                      "t" 'org-set-tags
                      "p" 'org-priority-up
                      "P" 'org-priority-down))

;; (evil-define-key 'insert org-mode-map
;;   (kbd "M-j") 'org-shiftleft
;;   (kbd "M-k") 'org-shiftright
;;   (kbd "M-H") 'org-metaleft
;;   (kbd "M-J") 'org-metadown
;;   (kbd "M-K") 'org-metaup
;;   (kbd "M-L") 'org-metaright)

(provide 'use-org)
