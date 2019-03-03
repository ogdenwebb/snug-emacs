;;; env-maps.el -*- lexical-binding: t; -*-

;; Define leader key
(defvar snug-leader "SPC"
  "Initial <leader> key for Evil mode.")

(defvar snug-non-leader "M-SPC"
  "Leader key for insert and Emacs(and some other, see `general-non-normal-states') Evil states.")

(defvar snug-localleader "SPC m"
  "Prefix for bindings that are specific to the major mode (filetype)")
; It also binds , by default to the SPC m map, so that you can access e.g. SPC m e through ,

;; Reverse mapping for keyboard layouts other than english
(use-package reverse-im
  :config
  ;; (add-to-list 'reverse-im-modifiers 'super)
  (add-to-list 'reverse-im-input-methods "russian-computer")
  (reverse-im-mode t))

(use-package general
  :config
  ;; TODO: arg short-names
  (general-evil-setup))

(use-package hydra
  :defer t)

(with-eval-after-load 'hydra
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

(provide 'env-maps)
