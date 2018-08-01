;;; Org-mode settings  -*- lexical-binding: t -*-

(use-package org
  :config
  (use-package org-download
    :after org
    :ensure t)

  ;; Fit image into the screen
  ;; (setq org-image-actual-width '(200))
  (setq org-image-actual-width 200)
  (setq org-startup-with-inline-images nil)
  ;; (setq org-image-actual-width (/ (display-pixel-width) 3))
  (setq org-startup-indented t
        org-imenu-depth 4)

  ;; Fontify
  (setq org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-ellipsis " ~ ")

  ;; Fontify the whole line for headings (with a background color).
  ;; (setq org-fontify-whole-heading-line t)

  (defun org-init-hook ()
    (interactive)
    (when (bound-and-true-p nlinum-mode)
      (nlinum-mode -1))
    (when (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1))
    ;; TODO: disable git-gutter
    ;; Enable line wrapping
    (visual-line-mode t)
    (visual-line-mode t)
    (turn-off-smartparens-mode)
    (show-paren-mode -1))

  (add-hook 'org-mode-hook 'org-init-hook)

  (setq org-highest-priority ?A
        org-lowest-priority ?D
        org-default-priority ?B)

  ;; TODO:
  ;; org-todo-keywords
  ;; org-todo-state-tags-triggers
  ;; :tangle yes
  ;; export html/pdf; see C-c C-e

  (custom-theme-set-faces
   elmax/custom-theme
   '(org-done ((t (:foreground "dimgray" :bold t :strike-through t))))
   '(org-headline-done ((t (:foreground "dimgray" :bold nil :strike-through t)))))

  ;; Fontify done checkbox items in org-mode
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
   'append)

  ;; Open links
  (setq org-link-frame-setup '((vm . vm-visit-folder)
                               (gnus . org-gnus-no-new-news)
                               (file . find-file)))

  (setq org-src-window-setup 'current-window)

  ;; Customize org todo keywrods
  (setq org-todo-keywords '((sequence "TODO(t)" "EXPLORE(e)" "ACTIVE(a)" "|" "DONE(d)" "CANCELED(c)")))
  ;; (setq org-fast-tag-selection-include-todo t)

  (use-package evil-org
    :ensure t
    :after (evil org)
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              (lambda ()
                (evil-org-set-key-theme '(navigation insert textobjects)))))
  :general
  (general-define-key :keymaps 'org-mode-map
                      :states '(normal)
                      ;; TODO: (??) bind M-ret to O/o
                      ;; C-c C-z
                      ;; C-c C-x f
                      ;; M-S-ret
                      ;; M-S-left,right
                      ;; M-S-up,down
                      ;; S-left S-right
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
                      "c"   'org-ctrl-c-ctrl-c
                      ;; "t" 'org-set-tags
                      "t"   'org-ctrl-c-ctrl-c
                      "p" 'org-priority-up
                      "P" 'org-priority-down)

  (general-define-key :keymaps 'org-mode-map
                      :states '(insert)
                      "RET" 'org-return))
                      ;; "RET" 'org-return-indent))

;; (evil-define-key 'insert org-mode-map
;;   (kbd "M-j") 'org-shiftleft
;;   (kbd "M-k") 'org-shiftright
;;   (kbd "M-H") 'org-metaleft
;;   (kbd "M-J") 'org-metadown
;;   (kbd "M-K") 'org-metaup
;;   (kbd "M-L") 'org-metaright)

(provide 'use-org)
