;; Vertico - VERTical Interactive COmpletion

(use-package vertico
  ;; :straight (:files (:defaults "extensions/*"))
  :hook (after-init . vertico-mode)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Tidy shadowed file names
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  :config
  (setq vertico-resize nil
        vertico-count 12)
  :general
  (general-def vertico-map
    "<escape>" #'abort-recursive-edit)
  )

;; ;; Use the `orderless' completion style. Additionally enable
;; ;; `partial-completion' for file path expansion. `partial-completion' is
;; ;; important for wildcard support. Multiple files can be opened at once
;; ;; with `find-file' if you enter a wildcard. You may also give the
;; ;; `initials' completion style a try.

(use-package orderless
  :after vertico
  :config
  (setq completion-styles '(basic partial-completion orderless)
        completion-category-defaults nil
        ;; completion-category-overrides '((file (styles basic partial-completion)))

        ;; completion-category-overrides '((command (styles basic partial-completion))
        ;;                                 (symbol (styles basic partial-completion))
        ;;                                 (variable (styles basic partial-completion)))
        )

  ;; Exclude some commands
  (defun without-orderless (fn & rest args)
    (let ((completion-styles '(basic partial-completion)))
      (apply fn args)))

    )

;; Consulting completing-read
(use-package consult
  :defer t
  :after vertico
  :config
  ;; To enable vertico in eval-expression buffer
  ;; see https://github.com/minad/vertico/issues/24

  ;; NOTE: prolly we can use corfu as well
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; Do not trigger auto preview and use keybinding instead
  (setq consult-preview-key (kbd "C-SPC")
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1))

(use-package consult-vertico
  :straight nil
  :after consult)

;; Consult integration for projectile
(use-package consult-projectile
  :after (consult projectile)
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

(use-package consult-dir)

;; Ignore case in completions
;; (setq read-file-name-completion-ignore-case t
;;       read-buffer-completion-ignore-case t)

;; Enrich existing commands with completion annotations
(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))

;; Add icons to completion candidates in Emacs
(use-package all-the-icons-completion
  :hook ((after-init . all-the-icons-completion-mode)
         (marginalia-mode . all-the-icons-completion-marginalia-setup)))

(when (eq snug-default-completion-system 'vertico)
  (use-package consult
    :straight nil
    :general
    ([remap apropos]                       #'consult-apropos)
    ([remap bookmark-jump]                 #'consult-bookmark)
    ([remap evil-show-marks]               #'consult-mark)
    ([remap goto-line]                     #'consult-goto-line)
    ([remap imenu]                         #'consult-imenu)
    ([remap list-buffers]                  #'consult-buffer)
    ([remap load-theme]                    #'consult-theme)
    ([remap locate]                        #'consult-locate)
    ([remap man]                           #'consult-man)
    ([remap recentf-open-files]            #'consult-recent-file)
    ([remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame)
    ([remap switch-to-buffer-other-window] #'consult-buffer-other-window)
    ([remap switch-to-buffer]              #'consult-buffer)
    ([remap yank-pop]                      #'consult-yank-pop)
    )

  (use-package consult-projectile
    :straight nil
    :general
    ([remap projectile-find-file] #'consult-projectile))
  )

;; Embark
;; TODO provides ivy-occur alternative
;; Emacs Mini-Buffer Actions Rooted in Keymaps
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  )

;; Embark integration for consult package
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))


(provide 'env-vertico)
