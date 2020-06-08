;;; Org-mode settings  -*- lexical-binding: t -*-


;; ;; Split current tree into new buffer
;; (defun split-and-indirect-orgtree ()
;;   "Splits window to the right and opens an org tree section in it"
;;   (interactive)
;;   (split-window-right)
;;   (org-tree-to-indirect-buffer)
;;   (windmove-right))

;; MAYBE: org-metaleft org-metadown org-metaup org-metaright

(setq org-directory "~/Documents/org")

;; TODO: use org-plus-contrib
;; prolly need hook instead of :config
(use-package org
  :straight org-plus-contrib
  :preface
  (defun snug/org-init-hook ()
    ;; Fit image into the screen
    ;; (setq org-image-actual-width '(600))
    (setq org-image-actual-width nil)
    (setq org-image-actual-width 200)
    (setq org-startup-with-inline-images t)
    ;; (setq org-image-actual-width (/ (display-pixel-width) 3))
    (setq org-startup-indented t
          org-startup-folded t
          org-imenu-depth 4
          org-tags-column 0)

    ;; todo:
    ;; (setq org-startup-truncated nil)
    ;; Agenda
    (setq org-log-done t
          org-agenda-files '("~/agenda.org"))

    ;; Fontify
    (setq org-fontify-done-headline t
          org-fontify-quote-and-verse-blocks t
          ;; org-pretty-entities nil
          org-hide-emphasis-markers t
          org-hide-leading-stars t
          org-hide-leading-stars-before-indent-mode t
          org-ellipsis " ~ ")

    (setq org-adapt-indentation nil)
    ;; Fontify the whole line for headings (with a background color).
    (setq org-fontify-whole-heading-line t)

    ;; Org priorities
    (setq org-highest-priority ?A
          org-lowest-priority ?D
          org-default-priority ?B)

    (setq org-priority-faces '((?A . org-todo)
                               (?B . warning)
                               (?C . success)
                               (?D . success)))

    ;; Done items
    (custom-theme-set-faces
     ;; snug-custom-theme
     'user
     '(org-done ((t (:foreground "dimgray" :bold t :strike-through t))))
     '(org-headline-done ((t (:foreground "dimgray" :bold nil :strike-through t)))))

    ;; Fontify done checkbox items in org-mode
    (font-lock-add-keywords
     'org-mode
     `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
     'append)

    ;; Open links
    (setq org-return-follows-link t
          org-link-frame-setup '((vm . vm-visit-folder)
                                 (gnus . org-gnus-no-new-news)
                                 (file . find-file)))

    ;; Customize org todo keywrods
    (setq org-todo-keywords '((sequence "TODO(t)" "EXPLORE(e)" "ACTIVE(a)" "|" "DONE(d)" "CANCELED(c)"))))
  ;; (setq org-fast-tag-selection-include-todo t))


  (defun snug/org-visual-hook ()
    ;; Set cursor-type for org-mode
    ;; (setq cursor-type 'bar)

    (when (bound-and-true-p nlinum-mode)
      (nlinum-mode -1))
    (when (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1))
    ;; TODO: disable git-gutter
    ;; Top padding
    ;; (setq header-line-format " ")
    ;; Enable line wrapping
    (visual-line-mode t)
    (turn-off-smartparens-mode)
    (show-paren-mode -1))

  :hook ((org-mode . snug/org-init-hook)
         (org-mode . snug/org-visual-hook)
         (org-mode . org-indent-mode))
  :config
  :general
  (general-define-key :keymaps 'org-mode-map
                      :states '(normal)
                      "RET" 'org-open-at-point
                      "gx"  'org-open-at-point
                      "t"   'org-todo
                      "T"   'org-insert-todo-heading
                      "M-T" 'org-insert-todo-subheading
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
                      :prefix snug-localleader
                      :states '(normal)
                      "a"   'org-agenda
                      "A"   'org-attach
                      "q"   'org-capture
                      "x"   'org-open-at-point

                      "c c"   'org-clock-cancel
                      "c g"   'org-clock-goto
                      "c e"   'org-evaluate-time-range
                      "c i"   'org-clock-in
                      "c I"   'org-clock-in-last
                      ;; "c j"   'org-clock-
                      "c o"   'org-clock-out
                      "c r"   'org-resolve-clocks
                      ;; "c R"   'org-
                      "d d"   'org-deadline
                      "d s"   'org-schedule
                      "d t"   'org-time-stamp
                      "d T"   'org-time-stamp-inactive

                      "m"   'org-ctrl-c-ctrl-c
                      "*"   'org-ctrl-c-star
                      "RET" 'org-ctrl-c-ret
                      "-"   'org-ctrl-c-minus

                      "l"   'org-shiftcontrolright
                      "h"   'org-shiftcontrolleft
                      "k"   'org-shiftcontrolup
                      "j"   'org-shiftcontroldown

                      "L"   'org-shiftright
                      "H"   'org-shiftleft
                      "K"   'org-shiftup
                      "J"   'org-shiftdown)

  ;; "p" 'org-priority-up
  ;; "P" 'org-priority-down)

  (general-define-key :keymaps 'org-mode-map
                      :states '(insert)
                      "RET" 'org-return))
;; "RET" 'org-return-indent))

(use-package org-tempo
  :straight nil
  :after org)

(use-package org-src
  :straight nil
  :after org
  :config
  (setq org-src-window-setup 'current-window
        org-edit-src-content-indentation 0
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t)
  )

(use-package org-superstar-mode
  :defer t
  :straight (:host github :repo "integral-dw/org-superstar-mode")
  :hook (org-mode . org-superstar-mode)
  :init
  ;; Pretty stars for org-mode
  (setq org-superstar-prettify-item-bullets t
        org-superstar-headline-bullets-list
        '("◉" "✸" "⚫" "○" "•")

        org-superstar-leading-bullet ?\s
        org-hide-leading-stars t

        org-superstar-item-bullet-alist
        '((?* . ?●)
          (?+ . ?➤)
          (?- . ?—)))
  )

;; TODO: icons are too small
(use-package org-fancy-priorities
  :disabled t
  :defer t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '((?A . "❗")
                                    (?B . "⬆")
                                    (?C . "⬇")
                                    (?D . "☕"))))

(use-package evil-org
  :straight (:host github :repo "Somelauw/evil-org-mode")
  :after (evil)
  :hook (org-mode . evil-org-mode)
  :config
  ;; TODO: doesn't work in tables
  ;; (evil-org-set-key-theme '(navigation insert textobjects shift todo heading))
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-download
  :after org
  :config
  (setq org-download-image-dir "~/Pictures/org/"))

(use-package org-variable-pitch
  :disabled t
  :hook (org-mode . org-variable-pitch-minor-mode)
  :config
  (setq org-variable-pitch-fixed-font "Merriweather"))

(provide 'use-org)
