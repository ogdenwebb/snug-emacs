;; OCaml
(use-package tuareg
  :mode ("\\.ml[4ilpy]?$" . tuareg-mode))


(use-package merlin
  :after (company tuareg)
  :hook (tuareg-mode . merlin-mode)
  :config
  (add-to-list 'company-backends 'merlin-company-backend))

  ;; (setq merlin-command 'opam))

(provide 'use-ocaml)
