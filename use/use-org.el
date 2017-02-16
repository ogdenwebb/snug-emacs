;; Org-mode

(use-package org
  :config
  ;; Fontify
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-src-fontify-natively t)
  (setq org-ellipsis " -")

  ;; done
  ;; (defun modify-org-done-face ()
  ;;   (setq org-fontify-done-headline t)
  ;;   (set-face-attribute 'org-done nil :strike-through t)
  ;;   (set-face-attribute 'org-headline-done nil :strike-through t))

  ;; (add-hook 'org-add-hook 'modify-org-done-face)
  (setq org-fontify-done-headline t)
  (custom-set-faces
    `(org-todo ((t (:foreground "#5485b6" :bold t))))
    `(org-done ((t (:foreground "dimgray" :bold t :strike-through t))))
    `(org-headline-done ((t (:foreground "dimgray" :bold nil :strike-through t)))))

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
                      "t" 'org-set-tags))

;; (evil-define-key 'insert org-mode-map
;;   (kbd "M-j") 'org-shiftleft
;;   (kbd "M-k") 'org-shiftright
;;   (kbd "M-H") 'org-metaleft
;;   (kbd "M-J") 'org-metadown
;;   (kbd "M-K") 'org-metaup
;;   (kbd "M-L") 'org-metaright)

(provide 'use-org)
