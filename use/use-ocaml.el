;; OCaml

;; ;; -- opam and utop setup --------------------------------
;; ;; Setup environment variables using opam
;; (dolist
;;     (var (car (read-from-string
;;                (shell-command-to-string "opam config env --sexp"))))
;;   (setenv (car var) (cadr var)))
;; ;; Update the emacs path
;; (setq exec-path (split-string (getenv "PATH") path-separator))
;; ;; Update the emacs load path
;; (push (concat (getenv "OCAML_TOPLEVEL_PATH")
;;               "/../../share/emacs/site-lisp") load-path)


(use-package tuareg
  :mode (("\\.ml[4ilpy]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode)))

(use-package merlin
  :after (company tuareg)
  :hook (tuareg-mode . merlin-mode)
  :config
  (add-to-list 'company-backends 'merlin-company-backend))

(use-package utop
  :after tuareg
  :config
  (setq utop-command "opam config exec -- utop -emacs"))
  ;; (setq merlin-command 'opam))

(use-package ocp-indent
  :after (tuareg)
  :config
  (add-hook 'tuareg-mode-hook 'ocp-indent-caml-mode-setup))

(defun ocaml/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
    (sp-local-pair 'tuareg-mode "'" nil :actions nil)
    (sp-local-pair 'tuareg-mode "`" nil :actions nil)))

(add-hook 'tuareg-mode-hook 'ocaml/post-init-smartparens)

(provide 'use-ocaml)
