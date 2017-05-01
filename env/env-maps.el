(use-package general
  :ensure t
  :config
  (setq general-default-keymaps 'evil-normal-state-map)

  (general-evil-setup)

  ;; Leader bindings
  (setq leader "SPC")
  (general-nvmap :prefix leader
                 "j" 'hydra-smartparens/body)

  (general-define-key :prefix leader
                      "1" 'colorpicker
                      "3" 'neotree-toggle
                      "5" 'hydra-flyspell/body
                      "f" 'counsel-recentf
                      "y" 'counsel-yank-pop
                      "k" 'counsel-ag
                      "l" 'counsel-bookmark
                      "i" 'find-file
                      "o" 'switch-to-buffer
                      "r" 'quickrun

                      "h k" 'describe-key
                      "h v" 'describe-variable
                      "h f" 'describe-function
                      "h F" 'describe-face
                      "h m" 'describe-mode

                      ;; TODO: Add focus and swap
                      "w c" 'delete-window
                      "w s" 'split-window-below
                      "w v" 'split-window-right
                      "w k" 'kill-this-buffer
                      "w K" 'kill-buffer

                      "m d" 'magit-diff
                      "m m" 'magit-status

                      "p" 'counsel-locate
                      "P" 'counsel-projectile-find-file)

  (general-define-key
   "<f6>" 'ivy-resume
   "/"    'swiper
   "C-j"  'parinfer-toggle-mode
   "M-h"  'evil-window-left
   "M-j"  'evil-window-down
   "M-k"  'evil-window-up
   "M-l"  'evil-window-right)

  ;; Bind ESC to jk
  (general-imap
   "j" (general-key-dispatch 'self-insert-command
         :name gl/evil-escape
         :timeout 0.25
         "k" 'evil-normal-state))

  ;; Evil bindings normal and visual
  (general-nvmap
   ;; "ga"  'align-regexp
   "j"   'evil-next-visual-line
   "k"   'evil-previous-visual-line)

  ;; Insert mode maps
  (general-imap
   "TAB" 'company-indent-or-complete-common
   "RET" 'newline-and-indent
   "C-j" 'parinfer-toggle-mode
   "C-k" 'company-complete-common-or-cycle
   "C-a" 'beginning-of-line
   "C-w" 'evil-delete-backward-word
   "C-e" 'end-of-line)

  ;; Dired
  (general-define-key :keymaps 'dired-mode-map
                      :states '(normal)
                      "h" 'dired-up-directory
                      "l" 'dired-find-alternate-file
                      "o" 'dired-sort-toggle-or-edit
                      "v" 'dired-toggle-marks
                      "m" 'dired-mark
                      "u" 'dired-unmark
                      "U" 'dired-unmark-all-marks
                      "c" 'dired-create-directory
                      "n" 'evil-search-next
                      "N" 'evil-search-previous
                      "q" 'kill-this-buffer)

  ;; Clojure mode
  (general-define-key :keymaps 'clojure-mode-map
                      :prefix leader
                      :states '(normal visual)
                      "e w" 'cider-eval-sexp-at-point
                      "e e" 'cider-eval-defun-at-point
                      "e r" 'cider-eval-last-sexp-to-repl
                      "e b" 'cider-eval-buffer
                      "e x" 'cider-eval-last-sexp-and-replace)

  (general-define-key :keymaps 'clojure-mode-map
                      :prefix leader
                      :states '(visual)
                      "e e" 'cider-eval-region))

;; (general-define-key :keymaps 'ivy-minibuffer-map
;;                     "C-n" 'ivy-previous-line-and-call
;;                     "C-p" 'ivy-next-line-and-call))

(provide 'env-maps)
