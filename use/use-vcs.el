;; Mercurial  -*- lexical-binding: t -*-
;; (use-package monky
;;   :config
;;   (setq monky-process-type 'cmdserver))

(setq-default vc-annotate-background nil
              vc-annotate-very-old-color nil)

;; Git
(use-package magit
  :preface
  ;; Ignore magit messages about version
  (advice-add #'magit-version :override #'ignore)
  :commands (magit-status magit-diff magit-blame magit-commit magit-status-only)
  :config

  ;; Enable pass integration
  ;; NOTE: create pass entry user^forge@api.github.com
  (setq magit-process-find-password-functions '(magit-process-password-auth-source))

  (defun magit-status-only ()
    "Invoke magit-status and delete other windows."
    (interactive)
    (magit-status)
    (delete-other-windows))

  ;; Disable arrows in section heading
  (setq magit-section-visibility-indicator nil
        magit-diff-buffer-file-locked nil
        magit-clone-set-remote.pushDefault t
        magit-remote-add-set-remote.pushDefault t
        magit-log-auto-more t
        ;; MAYBE:
        ;; magit-save-repository-buffers 'dontask))
        ))
  ;; (setq magit-diff-refine-hunk 'all))

;; Show source files' TODOs (and FIXMEs, etc) in Magit status buffer
(use-package magit-todos
  :disabled t
  :after magit
  ;; :hook (magit-status-mode . magit-todos-mode))
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
  (shut-up
    (magit-todos-mode t)))

;; Modes for Git-specific files
(use-package gitattributes-mode
             :disabled t
  :mode (("/\\.gitattributes\\'"  . gitattributes-mode)
         ("/info/attributes\\'"   . gitattributes-mode)
         ("/git/attributes\\'"    . gitattributes-mode)))

(use-package gitconfig-mode
             :disabled t
  :mode (("/\\.gitconfig\\'"      . gitconfig-mode)
         ("/\\.git/config\\'"     . gitconfig-mode)
         ("/modules/.*/config\\'" . gitconfig-mode)
         ("/git/config\\'"        . gitconfig-mode)
         ("/\\.gitmodules\\'"     . gitconfig-mode)
         ("/etc/gitconfig\\'"     . gitconfig-mode)))

(use-package gitignore-mode
             :disabled t
  :mode (("/\\.gitignore\\'"      . gitignore-mode)
         ("/info/exclude\\'"      . gitignore-mode)
         ("/git/ignore\\'"        . gitignore-mode)))

;; TODO: support github in my workflow
(use-package forge
  :after magit
  :preface
  (setq forge-add-default-bindings nil))

(use-package gist
  :commands (gist-list))

;; Highlight changes in git
(use-package git-gutter
  :hook (after-init . global-git-gutter-mode))

(use-package git-gutter-fringe
  :after git-gutter)
;; :config
;; (setq git-gutter-fr:side 'right-fringe))

;; Browse target page on github/bitbucket or other sources
(use-package browse-at-remote
  :commands (browse-at-remote bar-browse))

(provide 'use-vcs)
