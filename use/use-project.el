;; Project management and related extensions -*- lexical-binding: t -*-

(use-package project
  :ensure nil)

(defgroup snug-project nil
  "Snug settings for project management.")

(defcustom snug-project-find-file #'consult-project-extra-find
  "Function to use to find file in project."
  :type 'command
  :group 'snug-project)

(defcustom snug-project-switch-project #'project-switch-project
  "Function to use to swith projects."
  :type 'command
  :group 'snug-project)

(defcustom snug-project-find-find #'project-find-file
  "Function to find a file in the project."
  :type 'command
  :group 'snug-project)

(defcustom snug-project-replace #'project-query-replace-regexp
  "Function to find and replace in the project."
  :type 'command
  :group 'snug-project)

;; (defcustom snug-project-grep #'project-grep
;;   "Function to find a file in the project."
;;   :type 'function
;;   :group 'snug-project)

;; Consult integration with project.el
(use-package consult-project-extra
  :after consult
  :general
  :config)

;; Extension of project.el to detect project root with root file
(use-package project-rootfile
  :after project
  :config
  (add-to-list 'project-find-functions #'project-rootfile-try-detect t))

(provide 'use-project)
