;; TODO: add icons
;; TODO: big modeline config
;; TODO: check if all-the-icons installed
;; TODO: (??) disable mouse menu
;; TODO: add function to easy paste icons
;; TODO: add segment based on vc-state
;; TODO: use colors vars
;; TODO: rename my* segments to ???
  ;; Gray "#545c5e"

;; Telephone line
(use-package telephone-line
  ;; :load-path "dev/telephone-line"
  :ensure t
  :init
  ;; Need to display telephone-line in *Messages* buffer
  (defun recreate-message-buffer ()
    (cl-flet ((buffer-string* (buffer)
                (with-current-buffer buffer
                  (buffer-string))))
      (let ((msg (buffer-string* "*Messages*")))
        (kill-buffer "*Messages*")
        (message msg))))

  (add-hook 'after-init-hook #'recreate-message-buffer)
  :config

  ;; To create custom segments
  (require 'telephone-line-utils)


  ;; TODO: choose separator by name
  ;; Set default separators: choose either of them
  ;; (setq telephone-line-primary-left-separator 'telephone-line-flat)
  ;; (setq telephone-line-primary-right-separator 'telephone-line-flat)
  ;; OR
  ;; (setq telephone-line-primary-left-separator 'telephone-line-identity-left)
  ;; (setq telephone-line-primary-right-separator 'telephone-line-identity-right)
  ;; OR

  ;; Set subseparator
  (if window-system
      (progn
        (setq telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left)
        (setq telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right)))

  ;;;; Custom segments

  ;; Example of color string segment
  ;; (telephone-line-defsegment* my-color-segment
  ;;   (propertize "some-string" 'face `(:foreground "green")))


  ;; Display major mode
  ;; TODO: Rewrite using assoc and defvar #835d83
  (telephone-line-defsegment* my-major-mode-segment ()
    (let ((mode (cond
                  ((string= mode-name "Fundamental") "text")
                  ((string= mode-name "Emacs-Lisp") "elisp")
                  ((string= mode-name "Javascript-IDE") "js")
                  (t (downcase mode-name)))))
          ;; (icon (all-the-icons-icon-for-buffer)))
      ;; (concat
      ;;   (unless (symbolp icon) ;; This implies it's the major mode
      ;;     (format "%s "
      ;;             (propertize icon
      ;;                         'face `(:height 1.0 :family ,(all-the-icons-icon-family-for-buffer))
      ;;                         'display '(raise -0.1))))
      (propertize mode 'face `(:foreground "#9d81ba"))))
  ;; ;; Display name
  ;;   (propertize mode 'face `(:foreground "#9d81ba")))))

  ;; Display evil state
  (telephone-line-defsegment my-evil-segment ()
    (if (telephone-line-selected-window-active)
      (let ((tag (cond
                  ((string= evil-state "normal")    ":")
                  ((string= evil-state "insert")    ">")
                  ((string= evil-state "replace")   "r")
                  ((string= evil-state "visual")    "!")
                  ((string= evil-state "operator")  "=")
                  ((string= evil-state "motion")    "m")
                  ((string= evil-state "emacs")     "Emacs")
                  (t "-"))))
        (concat " " tag))))

  ;; Display buffer name
  (telephone-line-defsegment my-buffer-segment ()
    `(""
      ,(telephone-line-raw mode-line-buffer-identification t)))


  ;; Display current position in a buffer
  (telephone-line-defsegment* my-position-segment ()
    (if (telephone-line-selected-window-active)
        (if (eq major-mode 'paradox-menu-mode)
            (telephone-line-trim (format-mode-line mode-line-front-space))
          '(" %3l,%2c "))))

  ;; Exclude some buffers in modeline
  (defvar modeline-ignored-modes nil
    "List of major modes to ignore in modeline")

  (setq modeline-ignored-modes '("Dashboard"
                                 "Warnings"
                                 "Compilation"
                                 "EShell"
                                 "Debugger"
                                 "Quickrun"
                                 "REPL"
                                 "IELM"
                                 "Messages"))

  ;; Display modified status
  (telephone-line-defsegment my-modified-status-segment ()
    (when (and (buffer-modified-p) (not (member mode-name modeline-ignored-modes)))
        (propertize "+" 'face `(:foreground "#85b654"))))

  ;; Display read-only status
  (telephone-line-defsegment my-read-only-status-segment ()
    (when buffer-read-only
      ;; (propertize "ro" 'face `(:foreground "#dbac66"))
      (propertize (all-the-icons-octicon "key")
                  'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :foreground "dim gray")
                  'display '(raise 0.0))))

  ;; Display encoding system
  (telephone-line-defsegment my-coding-segment ()
    (let* ((code (symbol-name buffer-file-coding-system))
           (eol-type (coding-system-eol-type buffer-file-coding-system))
           (eol (cond
                 ((eq 0 eol-type) "unix")
                 ((eq 1 eol-type) "dos")
                 ((eq 2 eol-type) "mac")
                 (t ""))))
      (concat eol " ")))

  ;; TODO:
  ;; Hide vc backend in modeline
  (defadvice vc-mode-line (after strip-backend () activate)
      (when (stringp vc-mode)
        (let ((my-vc (replace-regexp-in-string "^ Git." "" vc-mode)))
          (setq vc-mode my-vc))))

  ;; Display current branch
  ;; TODO: move raise and etc into var
  (telephone-line-defsegment my-vc-segment ()
    ;; #6fb593 #4a858c
    (let (
          ;; (fg-color "#6fb593") ; kaolin-dark
          (fg-color "#9f84ae")) ; kaolin-galaxy
          ;; (fg-color "#709688")) ; kaolin-eclipse
      (when vc-mode
        ;; double format to prevent warnings in '*Messages*' buffer
          (format "%s %s"
                  (propertize (all-the-icons-octicon "git-branch")
                              'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :foreground ,fg-color)
                              'display '(raise 0.0))
                  (propertize
                    (format "%s"
                      (telephone-line-raw vc-mode t))
                    'face `(:foreground ,fg-color))))))

  (telephone-line-defsegment my-flycheck-segment ()
    ;; TODO: split errors and warnings
    (when (boundp 'flycheck-last-status-change)
      (pcase flycheck-last-status-change
        ('finished (if flycheck-current-errors
                       (let-alist (flycheck-count-errors flycheck-current-errors)
                         (let ((sum (+ (or .error 0) (or .warning 0))))
                           (format " %s: %s"
                                   (if .error "errors" "warnings")
                                   (number-to-string sum))))
                     ;; TODO:
                      " succesed"))
        ('running     " working...")
        ('no-checker  "")
        ('errored     " error")
        ('interrupted " interrupted"))))

  (require 'modeline-cubed)
  ;; (require 'modeline-flat)

  (telephone-line-mode 1))

(provide 'modeline-common)
