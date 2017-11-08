;; Mercurial
;; (use-package monky
;;   :config
;;   (setq monky-process-type 'cmdserver))

(use-package ediff
  :defer t
  :config
  (use-package evil-ediff))

; Git
(use-package magit
  :ensure t)

;; Highlight changes in git
(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode)
  (with-eval-after-load 'git-gutter
    (require 'git-gutter-fringe))

  (custom-set-variables
   ;; WARNING: "" contains tag space character to display line
   '(git-gutter:unchanged-sign "󠀠")
   '(git-gutter:added-sign "󠀠")
   '(git-gutter:modified-sign "󠀠")
   '(git-gutter:deleted-sign "󠀠"))

  ;; Color lines
  ;; (custom-theme-set-faces
  ;;  'kaolin
  ;; TODO:
  ;; (custom-theme-set-faces
  ;;  'kaolin-eclipse
  ;;  '(git-gutter:added ((t (:background "#54b685" :foreground "#54b685"))))
  ;;  ;; '(git-gutter:modified ((t (:background "#54b6b6" :foreground "#54b6b6"))))
  ;;  '(git-gutter:modified ((t (:background "#ab98b5" :foreground "#ab98b5"))))
  ;;  '(git-gutter:deleted ((t (:background "#d75f5f" :foreground "#d75f5f")))))
  :config
  (use-package git-gutter-fringe
    :after git-gutter
    :ensure t)
    ;; :config
    ;; (setq git-gutter-fr:side 'right-fringe))
  :general
  (general-nvmap
   "] h" 'git-gutter:next-hunk
   "[ h" 'git-gutter:previous-hunk))

;; (use-package git-gutter
;;   :commands (global-git-gutter-mode git-gutter-mode)
;;   :init
;;   (progn
;;     (add-hook 'prog-mode-hook 'git-gutter-mode)
;;     ;; TODO: break down nlinum V
;;     ;; (git-gutter:linum-setup)
;;   ;; :config
;;   ;; TODO: add live reload
;;   ;; (add-hook 'post-command-hook 'git-gutter:update-all-windows)
;;   ;; (add-hook 'focus-in-hook 'git-gutter:update-all-windows)

;;     (setq
;;      git-gutter:handled-backends '(git hg bzr svn)
;;      git-gutter:update-interval 2
;;      git-gutter:window-width 1
;;      git-gutter:diff-option "-w"
;;      git-gutter:hide-gutter t
;;      git-gutter:ask-p nil
;;      git-gutter:verbosity 0)

;;     (custom-set-variables
;;     ;; WARNING: "" contains tag space character to display line
;;      '(git-gutter:unchanged-sign "󠀠")
;;      '(git-gutter:added-sign "󠀠")
;;      '(git-gutter:modified-sign "󠀠")
;;      '(git-gutter:deleted-sign "󠀠"))

;;     ;; Color lines
;;     (custom-set-faces
;;       `(git-gutter:added ((t (:background "#54b685"))))
;;       `(git-gutter:modified ((t (:background "#acb370"))))
;;       `(git-gutter:deleted ((t (:background "#d75f5f"))))))
;;   :general
;;   (general-nvmap
;;    "] h" 'git-gutter:next-hunk
;;    "[ h" 'git-gutter:previous-hunk))

(provide 'use-vcs)
