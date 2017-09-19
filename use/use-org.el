;; Org-mode
;; TODO: add comments
;; TODO: read about fontify(priority, etc)
;; TODO: zc; fix for outline-hide-all
;; OR
;; TODO: migrate to evil-org

(use-package org
  :config
  (setq org-startup-indented t)
  ;; Fontify
  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-src-fontify-natively t)
  (setq org-ellipsis " ~ ")

  (defun org-init-hook ()
    (interactive)
    (nlinum-mode -1)
    ;; Enable line wrapping
    (visual-line-mode t))

  (add-hook 'org-mode-hook 'org-init-hook)

  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?D)
  (setq org-default-priority ?B)

  (custom-set-faces
   ;; '(org-todo ((t (:foreground "#5485b6" :bold t))))
   '(org-todo ((t (:foreground "#267fb5" :bold t))))

   '(org-done ((t (:foreground "dimgray" :bold t :strike-through t))))
   '(org-headline-done ((t (:foreground "dimgray" :bold nil :strike-through t)))))

  ;; Open links
  (setq org-link-frame-setup '((vm . vm-visit-folder)
                               (gnus . org-gnus-no-new-news)
                               (file . find-file)))
  (setq org-src-window-setup 'current-window)

  (use-package syndicate)
  :general
  (general-define-key :keymaps 'org-mode-map
                      :states '(normal)
                      "RET" 'org-open-at-point
                      "TAB" 'org-cycle
                      "za"  'org-cycle
                      "zA"  'org-shifttab
                      "zm"  'outline-hide-body
                      "zr"  'outline-show-all
                      "zo"  'outline-show-subtree
                      "zo"  'outline-show-all
                      "zc"  'outline-hide-subtree
                      "zc"  'outline-hide-all
                      "T"   'org-insert-todo-heading-respect-content)

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
