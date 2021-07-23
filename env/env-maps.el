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
  :hook (after-init . reverse-im-mode)
  :config
  ;; (add-to-list 'reverse-im-modifiers 'super)
  (add-to-list 'reverse-im-input-methods "russian-computer"))

(use-package general
  :demand t
  :config
  (general-evil-setup t))

(provide 'env-maps)
