;; Reverse mapping for keyboard layouts other than english
(use-package reverse-im
  :config
  ;; (add-to-list 'reverse-im-modifiers 'super)
  (add-to-list 'reverse-im-input-methods "russian-computer")
  (reverse-im-mode t))

(use-package general
  :config
  (general-evil-setup)
  (setq leader "SPC") ; set leader key

  ;; TODO: add treemacs
  (general-define-key ; :keymaps '(override)
                      :states '(normal)
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
                      "2" 'undo-tree-visualize
                      "3" 'treemacs
                      "4" 'imenu-list-smart-toggle
                      "5" 'hydra-flyspell/body
                      "6" 'ivy-resume
                      "k" 'counsel-ag
                      "r" 'quickrun

                      ;; Open things quickly
                      "o r" 'counsel-recentf
                      ;; "o f" 'counsel-file-jump
                      "o f" 'counsel-fzf
                      "o F" 'counsel-find-file
                      "o b" 'switch-to-buffer
                      "o l" 'counsel-bookmark
                      "o y" 'counsel-yank-pop

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

                      ;; TODO: add erase-buffer revert-buffer
                      "b p" 'previous-buffer
                      "b n" 'next-buffer
                      "b l" 'ivy-switch-buffer

                      ;; Help
                      "h a" 'apropos
                      "h k" 'describe-key
                      "h v" 'describe-variable
                      "h f" 'describe-function
                      "h F" 'describe-face
                      "h m" 'describe-mode
                      "h l" 'find-library
                      "h i" 'info

                      ;; Window management
                      "w c" 'delete-window
                      "w s" 'split-window-below
                      "w v" 'split-window-right
                      "w d" 'kill-this-buffer
                      "w D" 'kill-buffer

                      "w w" 'evil-window-next
                      "w h" 'evil-window-left
                      "w j" 'evil-window-down
                      "w k" 'evil-window-up
                      "w l" 'evil-window-right

                      "w H" 'evil-window-move-far-left
                      "w J" 'evil-window-move-far-down
                      "w K" 'evil-window-move-far-up
                      "w L" 'evil-window-move-far-right

                      "w m" 'minimize-window
                      "w M" 'maximize-window

                      ;; Git/Magit
                      "g d" 'magit-diff
                      "g s" 'magit-status
                      "g l" 'magit-log-current

                      ;; Projectile
                      "p p" 'projectile-switch-project
                      "p f" 'counsel-projectile-find-file
                      "p k" 'projectile-grep
                      "p r" 'projectile-replace
                      "p t" 'projectile-regenerate-tags)

  (general-define-key
   :keymaps '(normal visual)
   :prefix leader
   "t"   'google-translate-smooth-translate
   ;; Narrowing
   "n r" 'narrow-to-region
   "n d" 'narrow-to-defun
   "n p" 'narrow-to-page
   "n w" 'widen)

  ;; Normal state
  (general-define-key
   :states '(normal visual)
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

   "M-x"  'counsel-M-x
   ;; Drag stuff
   "C-h"  'drag-stuff-left
   "C-j"  'drag-stuff-down
   "C-k"  'drag-stuff-up
   "C-l"  'drag-stuff-right)

  ;; nvmap
  (general-define-key
   :keymaps '(normal visual)

   ;; evil-commentary
   "g c" 'evil-commentary
   "g y" 'evil-commentary-yank
   "g p" 'snug/evil-select-pasted

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

  ;; Bind ESC to jk in insert mode
  (general-define-key
   :keymaps '(insert)
   "j" (general-key-dispatch 'self-insert-command
         :name gl/evil-escape
         :timeout 0.25
         "k" 'evil-normal-state))

  ;; Company maps
  (general-define-key
   :keymaps '(company-active-map)
   "C-w"        nil
   "C-h"        #'company-quickhelp-manual-begin
   "C-n"        #'company-select-next
   "C-o"        #'company-search-kill-others
   "C-p"        #'company-select-previous
   "C-s"        #'company-filter-candidates
   "C-S-h"      #'company-show-doc-buffer
   "C-S-s"      #'company-search-candidates
   "C-SPC"      #'company-complete-common
   [return]     #'company-complete-selection
   [tab]        #'company-complete-common-or-cycle
   [backtab]    #'company-select-previous)

  (general-define-key
   :keymaps '(company-search-map)
   "C-n"    #'company-search-repeat-forward
   "C-p"    #'company-search-repeat-backward
   "C-s"    (lambda () (company-search-abort) (company-filter-candidates))
   [escape] #'company-search-abort)


  ;; Insert mode maps
  (general-define-key
   :states '(insert)
   "TAB" 'company-indent-or-complete-common
   ;; "TAB" 'company-indent-for-tab-command
   ;; "DEL" 'snug/smart-backspace
   "C-v" 'clipboard-yank
   "C-k" 'company-complete-common-or-cycle
   "C-a" 'beginning-of-line
   "C-w" 'evil-delete-backward-word
   "C-e" 'end-of-line)

  (general-define-key
   :keymaps 'prog-mode-map
   :states '(insert)
   "RET" 'snug/newline-and-indent)

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
                      ;; "n" 'evil-search-next
                      ;; "N" 'evil-search-previous
                      "q" 'kill-this-buffer)


  ;; Lisp modes
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

  ;; (general-define-key :keymaps '(python-mode-map nim-mode-map haskell-mode-map)
  ;;                     :states '(insert)
  ;;                     "RET" 'evil-ret-and-indent)

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
  ;; (general-define-key :keymaps 'term-mode-map
  ;;                     :states  '(insert)
  ;;                     "RET" 'term-send-input)

  ;; Org-mode
  (general-define-key :keymaps 'org-mode-map
                      :states  '(normal visual)
                      "M-h"  'evil-window-left
                      "M-j"  'evil-window-down
                      "M-k"  'evil-window-up
                      "M-l"  'evil-window-right)

  ;; Evil-surround
  (general-define-key :keymaps '(operator)
                      "s" 'evil-surround-edit
                      "S" 'evil-Surround-edit)

  (general-define-key :keymaps '(visual)
                      "S"  'evil-surround-region
                      "gS" 'evil-Surround-region)

  (general-define-key :keymaps '(haskell-mode-map)
                      :states '(insert)
                      "TAB"  'company-indent-or-complete-common))

;; (general-define-key :keymaps 'ivy-minibuffer-map
;;                     "C-n" 'ivy-previous-line-and-call
;;                     "C-p" 'ivy-next-line-and-call))

(provide 'env-maps)
