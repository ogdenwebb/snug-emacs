(use-package general
  :ensure t
  :config

  ;; Leader bindings
  (setq leader "SPC")

  (general-define-key :states 'normal
                      :prefix leader
                      ;; TODO: add load-theme somewhere
                      ;; TODO: mb add configuration opt or smth
                      ;; TODO: (??) add second leader
                      ;; TODO: counsel-linux-app
                      ;; counsel-mark-ring
                      ;; counsel-imenu
                      ;; counsel-descbinds
                      ;; counsel-unicode-char
                      ;; counsel-faces
                      "1" 'colorpicker
                      ;; TODO: mapundolist
                      ;; "2" 'undolist
                      "3" 'treemacs-toggle
                      "4" 'imenu-list-smart-toggle
                      "5" 'hydra-flyspell/body
                      "6" 'ivy-resume
                      "f" 'counsel-recentf
                      "y" 'counsel-yank-pop
                      "k" 'counsel-rg
                      "l" 'counsel-bookmark
                      ;; TODO: ?? i -> O
                      "I" 'counsel-find-file
                      "i" 'counsel-file-jump
                      "o" 'switch-to-buffer
                      "r" 'quickrun

                      ;; TODO: (??) "c e" "c s" for list errors(i.e. check err, syn)
                      ;; "e" 'flycheck-list-errors
                      ;; TODO: (??) move to lisp mode
                      ;; TODO: remap "e d"
                      "e e" 'eval-expression
                      "e p" 'eval-print-last-sexp
                      "e w" 'eval-last-sexp
                      "e d" 'eval-defun
                      "e b" 'eval-buffer

                      ;; Olivetti
                      ". ." 'olivetti-mode
                      ". [" 'olivetti-shrink
                      ". ]" 'olivetti-expand
                      ". m" 'olivetti-toggle-hide-mode-line

                      ;; TODO: maybe swap with magit(g - git)
                      "b p" 'previous-buffer
                      "b n" 'next-buffer
                      ;; "b l" 'list-buffers
                      "b l" '(ibuffer nil)

                      ;; Help
                      "h a" 'apropos
                      "h k" 'describe-key
                      "h v" 'describe-variable
                      "h f" 'describe-function
                      "h F" 'describe-face
                      "h m" 'describe-mode
                      "h l" 'find-library
                      "h i" 'info

                      ;; TODO: Add focus and swap
                      "w c" 'delete-window
                      "w s" 'split-window-below
                      "w v" 'split-window-right
                      "w d" 'kill-this-buffer
                      "w D" 'kill-buffer

                      "m d" 'magit-diff
                      "m m" 'magit-status
                      "m l" 'magit-log-current


                      ;; Projectile
                      "p f" 'counsel-projectile-find-file
                      "p k" 'projectile-grep
                      "p r" 'projectile-replace
                      "p t" 'projectile-regenerate-tags)

  (general-define-key
   :keymaps '(normal visual)

   "t"   'google-translate-smooth-translate
   ;; Narrowing
   "n r" 'narrow-to-region
   "n d" 'narrow-to-defun
   "n p" 'narrow-to-page
   "n w" 'widen)

  ;; Normal state
  (general-define-key
   :states 'normal
   ;; TODO:
   ;; "*" (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'symbol))))
   ;; "#" (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'word))))
   "g x"  'browse-url-at-point
   "/"    'swiper
   ;; Navigation between windows
   "M-h"  'evil-window-left
   "M-j"  'evil-window-down
   "M-k"  'evil-window-up
   "M-l"  'evil-window-right
   ;; Drag stuff
   "C-h"  'drag-stuff-left
   "C-j"  'drag-stuff-down
   "C-k"  'drag-stuff-up
   "C-l"  'drag-stuff-down)

  ;; nvmap
  (general-define-key
   :keymaps '(normal visual)

   ;; evil-commentary
   ;; "g c" 'evil-commentary
   ;; "g y" 'evil-commentary-yank

   ;; evil-lion
   "g a"  'evil-lion-left
   "g A"  'evil-lion-right

   "j"   'evil-next-visual-line
   "k"   'evil-previous-visual-line
   "C-a" 'evil-numbers/inc-at-pt
   "C-x" 'evil-numbers/dec-at-pt

   ;; git-gutter
   "] h" 'git-gutter:next-hunk
   "[ h" 'git-gutter:previous-hunk)

  ;; Bind ESC to jk
  (general-define-key
   :keymaps '(insert)
   "j" (general-key-dispatch 'self-insert-command
         :name gl/evil-escape
         :timeout 0.25
         "k" 'evil-normal-state))


  ;; Insert mode maps
  (general-define-key
   :keymaps '(insert)
   "TAB" 'company-indent-or-complete-common
   ;; "RET" 'evil-ret-and-indent
   "RET" 'reindent-then-newline-and-indent
   "DEL" 'elmax/smart-backspace
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
                      "e e" 'cider-eval-region)

  (general-define-key :keymaps 'emacs-lisp-mode-map
                      :prefix leader
                      :states '(visual)
                      "e r" 'eval-region)
  ;; Flycheck
  (general-define-key :keymaps 'flycheck-mode-map
                      :states '(normal)
                      "] e" 'flycheck-next-error
                      "[ e" 'flycheck-previous-error)

  ;; EShell
  ;; (general-define-key :keymaps 'eshell-mode-map
  ;;                     :states  '(insert)
  ;;                     "RET" 'eshell-send-input)

  ;; Term-mode
  ;; TODO:
  (general-define-key :keymaps 'term-mode-map
                      :states  '(insert)
                      "RET" 'term-send-input)

  ;; Org-mode
  (general-define-key :keymaps 'org-mode-map
                      :states  '(normal visual)
                      "M-h"  'evil-window-left
                      "M-j"  'evil-window-down
                      "M-k"  'evil-window-up
                      "M-l"  'evil-window-right))

;; (general-define-key :keymaps 'ivy-minibuffer-map
;;                     "C-n" 'ivy-previous-line-and-call
;;                     "C-p" 'ivy-next-line-and-call))

(provide 'env-maps)
