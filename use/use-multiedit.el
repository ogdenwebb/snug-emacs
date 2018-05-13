;; Multiedit
(use-package evil-multiedit
  :disabled t
  :config
  ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)

  :general
  (general-vmap "R" 'evil-multiedit-match-all
                "C-M-D" 'evil-multiedit-restore)

  (general-nvmap "M-d" 'evil-multiedit-match-and-next
                 "M-D" 'evil-multiedit-match-and-prev)

  (general-mmap "RET" 'evil-multiedit-toggle-or-restrict-region)

  (general-define-key :keymaps 'evil-multiedit-state-map
                      "RET" 'evil-multiedit-toggle-or-restrict-region
                      "C-p" 'evil-multiedit-prev
                      "C-n" 'evil-multiedit-next)

  (general-define-key :keymaps 'evil-multiedit-insert-state-map
                      "C-p" 'evil-multiedit-prev
                      "C-n" 'evil-multiedit-next))


(provide 'use-multiedit)
