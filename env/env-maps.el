;;; env-maps.el -*- lexical-binding: t; -*-

;; Define leader key
(defvar snug-leader "SPC"
  "Initial <leader> key for Evil mode.")

;; MAYBE: Or M-m
(defvar snug-non-leader "M-SPC"
  "Leader key for insert and Emacs(and some other, see `general-non-normal-states') Evil states.")

;; Major mode leader
(defvar snug-localleader "SPC m"
  "Prefix for bindings that are specific to the major mode (filetype)")

;; Reverse mapping for keyboard layouts other than english
(use-package reverse-im
  :defer t
  :hook (after-init . reverse-im-mode)
  :config
  ;; (add-to-list 'reverse-im-modifiers 'super)
  (add-to-list 'reverse-im-input-methods "russian-computer"))

(use-package general
  :config
  (general-evil-setup t))

(use-package lv
  :defer t)

(use-package hydra
  :defer 2
  :config
  (defhydra hydra-buffer (:color blue :columns 3)
    "
                Buffers :
    "
    ("n" next-buffer "next" :color red)
    ("b" ivy-switch-buffer "switch")
    ("B" ibuffer "ibuffer")
    ("p" previous-buffer "prev" :color red)
    ("C-b" buffer-menu "buffer menu")
    ("N" evil-buffer-new "new")
    ("d" kill-this-buffer "delete" :color red)
    ;; don't come back to previous buffer after delete
    ("D" (progn (kill-this-buffer) (next-buffer)) "Delete" :color red)
    ("s" save-buffer "save" :color red)))

(use-package use-package-hydra
  :after (hydra))

(use-package pretty-hydra
  :after (hydra))

(use-package hydra-posframe
  :straight (:host github :repo "Ladicle/hydra-posframe" :files ("*.el"))
  :after (posframe hydra)
  :hook (after-init . hydra-posframe-mode)
  :config
  (setq hydra-posframe-border-width 15))

(use-package major-mode-hydra
  :after (hydra)
  :preface
  (setq major-mode-hydra-separator " ")

  (setq major-mode-hydra-title-generator
        '(lambda (mode)
           (s-concat ; "\n"
            ;; (s-repeat 2 " ")
            (all-the-icons-icon-for-mode mode :v-adjust 0.05 :face 'font-lock-function-name-face)
            " "
            "Emacs lisp commands")))

  :config

  (major-mode-hydra-define emacs-lisp-mode nil
    ("Eval"
     (("b" eval-buffer "buffer")
      ("e" eval-defun "defun")
      ("r" eval-region "region"))
     "REPL"
     (("I" ielm "ielm"))
     "Test"
     (("t" ert "prompt")
      ("T" (ert t) "all")
      ("F" (ert :failed) "failed"))
     "Doc"
     (("d" describe-foo-at-point "thing-at-pt")
      ("f" describe-function "function")
      ("v" describe-variable "variable")
      ("i" info-lookup-symbol "info lookup")))))

(provide 'env-maps)
