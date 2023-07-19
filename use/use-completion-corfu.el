;; Corfu - Completion Overlay Region FUnction  -*- lexical-binding: t; -*-

(use-package corfu
  :elpaca (corfu :files (:defaults "extensions/*"))
  :config
  (setq corfu-quit-no-match 'separator ;; Automatically quit if there is no match
        ;; corfu-auto t                   ;; Enable auto completion
        ;; corfu-separator ?\s            ;; Orderless field separator
        corfu-cycle t                  ;; Enable cycling for `corfu-next/previous'
        corfu-preview-current nil      ;; Preview currently selected candidate.
        corfu-preselect-first nil      ;; Disable candidate preselection
        )

  ;; Optional customizations
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  ;; (corfu-echo-documentation nil) ;; Do not show documentation in the echo area

  (setq tab-always-indent 'complete)
        ;; completion-cycle-threshold t

  ;; Corfu maps
  ;; TODO: make C-n/C-p to select next candidate in popup instead of completion
  (general-define-key
   :keymaps 'corfu-map
   "C-n"     #'corfu-next
   "C-p"     #'corfu-previous
   [tab]     #'corfu-next
   "S-TAB"   #'corfu-previous
   [backtab] #'corfu-previous
   "C-;"     #'corfu-popupinfo-toggle
   "SPC"     #'corfu-insert-separator
   "RET"     #'corfu-insert
   [return]  #'corfu-insert)

  ;; You may want to enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)))

;; Documentation popup for Corfu
(use-package corfu-popupinfo
  :elpaca nil
  :after corfu
  :config
  (setq corfu-popupinfo-delay 0.2
        corfu-popupinfo-max-width 80
        corfu-popupinfo-max-height 40))

(use-package kind-icon
  :after corfu
  :config
  ;; Text-based Icons
  (setq kind-icon-use-icons t
        kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'use-completion-corfu)
