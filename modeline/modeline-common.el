;;; Custom modeline based on telephone-line package.  -*- lexical-binding: t; -*-

;; TODO: check if all-the-icons installed
;; TODO: add function to easy paste icons
;; TODO: use colors vars
;; TODO: rename my* segments to ???
;; Gray "#545c5e"

;; Telephone line
(use-package telephone-line
  ;; :load-path "dev/telephone-line"
  :defer t
  :hook ((after-init . telephone-line-mode)
         (after-init . recreate-message-buffer))
  :config
  ;; Need to display telephone-line in *Messages* buffer
  (defun recreate-message-buffer ()
    (cl-flet ((buffer-string* (buffer)
                              (with-current-buffer buffer
                                (buffer-string))))
      (let ((msg (buffer-string* "*Messages*")))
        (kill-buffer "*Messages*")
        (message msg))))

  ;; (add-hook 'after-init-hook #'recreate-message-buffer)

  ;; To create custom segments
  (require 'telephone-line-utils)
  (require 'let-alist)

  ;; Set subseparator
  ;; TODO: function to choose separator by name
  (if window-system
      (progn
        (setq telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left
              telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right)))

  ;;; Custom segments

  ;; Display major mode
  ;; TODO: rewrite with var/macro
  (telephone-line-defsegment* my-major-mode-segment ()
    (let* ((name (if (or (version< emacs-version "28.0") (stringp mode-name))
                     mode-name
                   (car mode-name)))
           (mode (cond
                 ((string= name "Fundamental") "text")
                 ((string= name "Emacs-Lisp") "elisp")
                 ((string= name "Javascript-IDE") "js")
                 ((string= name "undo-tree-visualizer") "undotree")
                 ((string= name "C++//l") "cpp")
                 (t (downcase name)))))
      (propertize mode 'face `font-lock-string-face)))

  ;; TODO: add raise or v-adjust
  (telephone-line-defsegment* my-major-mode-segment-icon ()
    (let* ((name (if (or (version< emacs-version "28.0") (stringp mode-name))
                     mode-name
                   (car mode-name)))
           (mode (cond
                 ((string= name "Fundamental") "text")
                 ((string= name "Emacs-Lisp") "elisp")
                 ((string= name "ELisp") "elisp")
                 ((string= name "Javascript-IDE") "js")
                 ((string= name "undo-tree-visualizer") "undotree")
                 ((string= name "C++//l") "cpp")
                 (t (downcase name))))
          (icon (all-the-icons-icon-for-mode major-mode :v-adjust 0.0 :height 0.8 :face font-lock-string-face)))
      (concat
       (when
           (and (not (eq major-mode (all-the-icons-icon-for-mode major-mode)))
                (telephone-line-selected-window-active))
         (format "%s " icon))
       (propertize mode 'face `font-lock-string-face))))


  (telephone-line-defsegment my-evil-segment ()
    "Display evil state as text symbol."
    (when (telephone-line-selected-window-active)
      (let ((tag (cond
                  ((not (boundp 'evil-state))       "")
                  ((string= evil-state "normal")    ":")
                  ((string= evil-state "insert")    ">")
                  ((string= evil-state "replace")   "~")
                  ((string= evil-state "visual")    "!")
                  ((string= evil-state "operator")  "=")
                  ((string= evil-state "motion")    "m")
                  ((string= evil-state "emacs")     "Emacs")
                  ((string= evil-state "multiedit") "ME")
                  (t "-"))))
        (format " %s" tag))))

  (telephone-line-defsegment my-evil-segment-icon ()
    "Display evil state as icon with all-the-icons."
    (let ((tag (cond
                ((string= evil-state "normal")    (all-the-icons-faicon "magic"))
                ((string= evil-state "insert")    (all-the-icons-faicon "pencil"))
                ((string= evil-state "replace")   (all-the-icons-faicon "eraser"))
                ((string= evil-state "visual")    (all-the-icons-faicon "clipboard"))
                ;; TODO:
                ;; ((string= evil-state "operator")  (all-the-icons-faicon "dot-circle-o"))
                ((string= evil-state "motion")    (all-the-icons-faicon "angle-double-right"))
                ((string= evil-state "emacs")     (all-the-icons-fileicon "org"))
                (t "-"))))
      (format " %s" tag)))

  ;; Display buffer name
  (telephone-line-defsegment my-buffer-segment ()
    (format "%s %s"
            (propertize (all-the-icons-fileicon "elisp")
                        'face `(:family ,(all-the-icons-fileicon-family) :height 1.0)
                        'display '(raise 0.0))
            (propertize
             (format "%s"
                     (telephone-line-raw mode-line-buffer-identification t)))))
  ;; 'face `(:foreground ,fg-color))))

  ;; Display current position in a buffer
  (declare-function column-number-at-pos "env-fun")

  ;; (telephone-line-defsegment my-position-segment ()
  ;;   (let ((line (line-number-at-pos (point)))
  ;;         (column (column-number-at-pos (point))))
  ;;     (format " %3d:%2d " line column)))

  (telephone-line-defsegment my-position-segment (&optional lines columns)
     "Position segment. Optional args set padding on lines/columns."
     (when (telephone-line-selected-window-active)
       (let* ((l (number-to-string (if lines lines 3)))
              (c (number-to-string (if columns columns 2))))
         (if (eq major-mode 'paradox-menu-mode)
             (telephone-line-raw mode-line-front-space t)
           `(,(concat " %" l "l:%" c "c"))))))


  ;; Exclude some buffers in modeline
  (defvar modeline-ignored-modes '("Dashboard"
                                 "Warnings"
                                 "Compilation"
                                 "EShell" "Eshell"
                                 "Debugger"
                                 "Quickrun"
                                 "REPL"
                                 "IELM"
                                 "Messages"
                                 "Interactive-Haskell")
    "List of major modes to ignore in modeline")

  ;; Display modified status
  (telephone-line-defsegment my-modified-status-segment ()
    (when (and (buffer-modified-p) (not (member mode-name modeline-ignored-modes)) (not buffer-read-only))
      (propertize "+" 'face `(:foreground "#85b654"))))

  ;; Display read-only status
  (telephone-line-defsegment my-read-only-status-segment ()
    (when (and buffer-read-only (telephone-line-selected-window-active))
      ;; (propertize "ro" 'face `(:foreground "#dbac66"))
      (propertize (all-the-icons-octicon "key")
                  'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :foreground "dim gray")
                  'display '(raise 0.0))))

  ;; Display encoding system
  (telephone-line-defsegment my-coding-segment ()
    (when (telephone-line-selected-window-active)
      (let* ((code (symbol-name buffer-file-coding-system))
             (eol-type (coding-system-eol-type buffer-file-coding-system))
             (eol (cond
                   ((eq 0 eol-type) "unix")
                   ((eq 1 eol-type) "dos")
                   ((eq 2 eol-type) "mac")
                   (t "-"))))
        (format  "%s " eol))))

  ;; TODO:
  ;; Hide vc backend in modeline
  (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((my-vc (replace-regexp-in-string "^ Git." "" vc-mode)))
        (setq vc-mode my-vc))))

  ;; Display current branch
  ;; TODO: move raise and etc into var
  (telephone-line-defsegment my-vc-segment ()
    (when (and vc-mode (telephone-line-selected-window-active))
      ;; double format to prevent warnings in '*Messages*' buffer
      (format "%s %s"
              (propertize (all-the-icons-octicon "git-branch")
                          'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :foreground ,(face-foreground 'font-lock-variable-name-face))
                          'display '(raise 0.0))
              (propertize
               (format "%s"
                       (telephone-line-raw vc-mode t))
               'face `(:foreground ,(face-foreground 'font-lock-variable-name-face))))))

  ;; ;; TODO: free visual selection
  ;; ;; TODO: the segment doesn't update in real-time

  (telephone-line-defsegment selection-info ()
    "Information about the size of the current selection, when applicable.
  Supports both Emacs and Evil cursor conventions."
    (when (or mark-active
              (and (bound-and-true-p evil-local-mode)
                   (eq 'visual evil-state)))
      (let* (
             ;; (lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
             (lines (count-lines (region-beginning) (region-end)))
             (chars (- (1+ (region-end)) (region-beginning)))
             (cols (1+ (abs (- (column-number-at-pos (region-end))
                               (column-number-at-pos (region-beginning))))))
             (evil (and (bound-and-true-p evil-state) (eq 'visual evil-state)))
             (rect (or (bound-and-true-p rectangle-mark-mode)
                       (and evil (eq 'block evil-visual-selection))))
             (multi-line (or (> lines 1) (and evil (eq 'line evil-visual-selection)))))
        (cond
         (rect (format "%d×%d" lines (if evil cols (1- cols))))
         (multi-line (format "%dL" lines))
         (t (format "%d" (if evil chars (1- chars))))))))

  ;; (propertize (all-the-icons-octicon "key")
  ;;             'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :foreground "dim gray")
  ;;             'display '(raise 0.0))

  (telephone-line-defsegment my-flycheck-segment ()
    (when (and (telephone-line-selected-window-active) (boundp 'flycheck-last-status-change))
      (pcase flycheck-last-status-change
        ('finished (if flycheck-current-errors
                       (let-alist (flycheck-count-errors flycheck-current-errors)
                         (let ((errs (or .error 0))
                               (warns (or .warning 0)))
                           (concat
                            (when (> errs 0)
                              (format "%s %s"
                                      (propertize
                                       (all-the-icons-faicon "exclamation-triangle")
                                       'face `(:family ,(all-the-icons-faicon-family) :height 1.0 :foreground ,(face-foreground 'error))
                                       'display '(raise 0.0))
                                      (number-to-string errs)))
                            " "
                            (when (> warns 0)
                              (format "%s %s"
                                      (propertize
                                       (all-the-icons-faicon "exclamation-triangle")
                                       'face `(:family ,(all-the-icons-faicon-family) :height 1.0 :foreground ,(face-foreground 'warning))
                                       'display '(raise 0.0))
                                      (number-to-string warns))))
                           ))
                     ;; TODO: icons
                     " succeed"))
        ('running     " working...")
        ('no-checker  "")
        ('errored     " error")
        ('interrupted " interrupted"))))

  (telephone-line-defsegment my-words-count-segment ()
    (format "%d" (count-words (point-min) (point-max))))

  (require 'modeline-cubed)
  ;; (require 'modeline-flat)
  )

(provide 'modeline-common)
