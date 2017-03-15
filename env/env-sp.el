;; TODO:
;; Add move to last position

(use-package hydra
  :config
  (defhydra hydra-smartparens ()
    "Smartparens"
    ("k" sp-kill-sexp "Kill")
    ("y" sp-copy-sexp "Copy")
    ("E" sp-end-of-sexp "End")
    ("A" sp-beginning-of-sexp "Beginning")
    ("{" sp-beginning-of-previous-sexp "Beginning prev")
    ("}" sp-beginning-of-next-sexp "Beginning next")
    ("U" sp-backward-up-sexp "Backward up")
    ("u" undo "Undo")
    ("e" sp-up-sexp "Up")
    ("d" sp-down-sexp "Down")
    ("D" sp-backward-down-sexp "Backward down")
    ("F" sp-forward-sexp "Forward sexp")
    ("B" sp-backward-sexp "Backward sexp")
    ("p" sp-previous-sexp "Previous")
    ;; ("w" (sp-wrap-with-pair "(") "Wrap")
    ("W" sp-unwrap-sexp "Unwrap")
    ("n" sp-next-sexp "Next")
    ("J" sp-join-sexp "Join")
    ("j" sp-split-sexp "Split")
    ("c" sp-convolute-sexp "Convolute")
    ("C" sp-absorb-sexp "Absorb")
    ("r" sp-raise-sexp "Raise")
    ("R" sp-rewrap-sexp "Rewrap")
    ("s" sp-splice-sexp "Splice")
    ("S" sp-splice-sexp-killing-around "Splice killing around")
    ("t" sp-transpose-sexp "Transpose")
    ("l" sp-forward-slurp-sexp "Forward slurp")
    ("H" sp-backward-slurp-sexp "Backward slurp")
    ("L" sp-forward-barf-sexp "Forward barf")
    ("h" sp-backward-barf-sexp "Backward barf")
    ("f" sp-forward-symbol "Forward")
    ("b" sp-backward-symbol "Backward")
    ("x" sp-extract-after-sexp "Extract")
    ("X" sp-extract-before-sexp "Extract before")
    ("<" sp-add-to-previous-sexp "Add to previous")
    (">" sp-add-to-next-sexp "Add to next")
    ("q" nil "Quit")))

(use-package smartparens
  :init
  (smartparens-global-mode 1)
  :config
  ;; (!!) Read: sp-cheat-sheet
  (use-package smartparens-config)
  (sp-local-pair 'clojure-mode "(" ")" :actions nil)
  (sp-local-pair 'clojurescript-mode "(" ")" :actions nil)
  (sp-local-pair 'emacs-lisp-mode "(" ")" :actions nil)

  (sp-pair "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (setq-default sp-autoskip-closing-pair 'always-end)

  (sp-pair "\"" nil :actions '(:rem escape)))

(provide 'env-sp)
