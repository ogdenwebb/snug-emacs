;; Use hydra to create useful menu -*- lexical-binding: t -*-
(use-package hydra
  :defer 2
  :config
  (defhydra hydra-buffer (:color blue :columns 3)
    "
                Buffers :
    "
    ("n" next-buffer "next" :color red)
    ("b" switch-to-buffer "switch")
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
  :ensure (:repo "https://github.com/Ladicle/hydra-posframe" :files ("*.el"))
  :after (posframe hydra)
  :hook (elpaca-after-init . hydra-posframe-mode)
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

(provide 'env-hydra)
