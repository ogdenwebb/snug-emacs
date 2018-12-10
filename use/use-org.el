;;; Org-mode settings  -*- lexical-binding: t -*-

;; TODO:
;; org-todo-keywords
;; org-todo-state-tags-triggers
;; :tangle yes
;; export html/pdf; see C-c C-e
;;
;; TODO: bind
;; C-c C-z
;; C-c C-x f
;; M-S-ret
;; M-S-left,right
;; M-S-up,down
;; S-left S-right

(use-package org
  :config
  ;; Fit image into the screen
  ;; (setq org-image-actual-width '(600))
  (setq org-image-actual-width nil)
  ;; (setq org-image-actual-width 200)
  (setq org-startup-with-inline-images t)
  ;; (setq org-image-actual-width (/ (display-pixel-width) 3))
  (setq org-startup-indented t
        org-startup-folded t
        org-imenu-depth 4
        org-tags-column 0)

  ;; Fontify
  (setq org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-hide-leading-stars t
        org-hide-leading-stars-before-indent-mode t
        org-ellipsis " ~ ")

  (setq org-adapt-indentation nil)
  ;; Fontify the whole line for headings (with a background color).
  (setq org-fontify-whole-heading-line t)

  (defun snug/org-init-hook ()
    (interactive)
    (when (bound-and-true-p nlinum-mode)
      (nlinum-mode -1))
    (when (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1))
    ;; TODO: disable git-gutter
    ;; Enable line wrapping
    (visual-line-mode t)
    (turn-off-smartparens-mode)
    (show-paren-mode -1))

  (add-hook 'org-mode-hook 'snug/org-init-hook)

  (setq org-highest-priority ?A
        org-lowest-priority ?D
        org-default-priority ?B)

  (custom-theme-set-faces
   snug/custom-theme
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
                      "c"   'org-ctrl-c-ctrl-c
                      ;; "t" 'org-set-tags
                      "t"   'org-ctrl-c-ctrl-c)
                      ;; "p" 'org-priority-up
                      ;; "P" 'org-priority-down)

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

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list
        '("◉" "✸" "⚫" "○" "•")))

(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(navigation insert textobjects)))))

(use-package org-download
  :after org)

(use-package org-variable-pitch
  :disabled
  :after org
  :config
  (setq org-variable-pitch-fixed-font "Philosopher")
  (add-hook 'org-mode-hook 'org-variable-pitch-minor-mode))

(provide 'use-org)
