;; OCaml
(use-package tuareg
  :mode (("\\.ml[4ilpy]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode))
  :config
  (setq tuareg-support-metaocaml t))

(use-package merlin
  ;; :after (company tuareg)
  :hook (tuareg-mode . merlin-mode)
  :config
  (add-to-list 'company-backends 'merlin-company-backend))

(use-package merlin-eldoc
  :hook ((reason-mode tuareg-mode caml-mode) . merlin-eldoc-setup))

(use-package utop
  :after tuareg
  :config
  (setq utop-command "opam config exec -- utop -emacs"))
;; (setq merlin-command 'opam))

(use-package ocp-indent
  :hook (tuareg-mode . ocp-indent-caml-mode-setup))

(use-package flycheck-ocaml
  :after (tuareg flycheck))

(defun ocaml/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
    (sp-local-pair 'tuareg-mode "'" nil :actions nil)
    (sp-local-pair 'tuareg-mode "`" nil :actions nil)))

(add-hook 'tuareg-mode-hook 'ocaml/post-init-smartparens)

(provide 'use-ocaml)
