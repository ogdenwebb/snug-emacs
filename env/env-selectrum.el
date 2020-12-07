;; Better solution for incremental narrowing in Emacs.  -*- lexical-binding: t -*-

(use-package selectrum
  :hook (after-init . selectrum-mode)
  :config
  (setf (alist-get "<escape>" selectrum-minibuffer-bindings) #'abort-recursive-edit)
  )

;; To make sorting and filtering more intelligent
(use-package selectrum-prescient
  :hook (selectrum-mode . selectrum-prescient-mode)
  )

(with-eval-after-load 'selectrum

  (cl-defmacro selectrum-make-action ((&rest args) &body body)
    (declare (indent 1))
    `(lambda ()
       (interactive)
       (put 'quit 'error-message "")
       (run-at-time nil nil
                    (lambda (,@args)
                      (put 'quit 'error-message "Quit")
                      (with-demoted-errors "Error: %S"
                        ,@body))
                    ,@(seq-take
                       `((if selectrum--refined-candidates (nth selectrum--current-candidate-index selectrum--refined-candidates))
                         selectrum--refined-candidates
                         (selectrum-get-current-input)
                         selectrum--current-candidate-index)
                       (length args)))
       (abort-recursive-edit)))

  (defvar selectrum-search-rg-history nil)

  (defun sel/rg ()
    "Search like 'counsel-rg'.

Default, search for current directory, if the input begin with 'p ' then
will search current project, if begin with 'o ' then will search org-directory.

'C-c C-o' to pop the rg.el's Occur view, make sure package `rg' is installed."
    (interactive)
    (unless (executable-find "rg")
      (user-error "ripgrep must be installed."))
    (let* (type
           input
           (dir default-directory)
           (word (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (let* ((sym (symbol-at-point)) (symn (symbol-name sym)))
                     (if (and sym (> 50 (length symn) 3)) symn nil))))
           (command (if (memq system-type '(ms-dos windows-nt))
                        "rg -M 240 --with-filename --no-heading --line-number --color never -S -e <R> ."
                      "rg -M 240 --with-filename --no-heading --line-number --color never -S -e <R>"))
           (cands (lambda (in)
                    (let ((msg)
                          (prop (lambda (cs)
                                  (mapcar (lambda (c)
                                            (when (string-match "\\`\\([^:]+\\):\\([^:]+\\):" c)
                                              (add-face-text-property (match-beginning 1) (match-end 1) 'compilation-info nil c)
                                              (add-face-text-property (match-beginning 2) (match-end 2) '(:underline t :inherit compilation-line-number) nil c))
                                            c)
                                          cs))))
                      (cond
                       ;; search current project
                       ((string-prefix-p "p " in)
                        (cond ((not (project-current))
                               (setq msg "This is not in a project."))
                              ((< (length in) 5)
                               (setq msg "Search in current project, input should more than 3."))
                              (t
                               (setq type 'project)
                               (setq dir (cdr (project-current)))
                               (setq in (cl-subseq in 2)))))
                       ;; search org-directory
                       ((string-prefix-p "o " in)
                        (cond ((not (file-exists-p org-directory))
                               (setq msg "Org Directory not exist?"))
                              ((< (length in) 5)
                               (setq msg "Search in org-directory, input should more than 3."))
                              (t
                               (setq type 'org)
                               (setq dir org-directory)
                               (setq in (cl-subseq in 2)))))
                       ;; search current directory
                       (t (if (< (length in) 3)
                              (setq msg "Input should more than 3."))
                          (setq type nil)
                          (setq dir default-directory)))
                      ;; take space in INPUT as .*?
                      ;; take m-space as [[:blank:]]
                      (setq input
                            (replace-regexp-in-string
                             " +" "[[:blank:]]"
                             (replace-regexp-in-string
                              "\\([^ ]\\) \\([^ ]\\)" "\\1.+?\\2"
                              (string-trim in))))
                      (if msg
                          (prog1 nil
                            (setq-local selectrum-refine-candidates-function
                                        (lambda (_ __) (list msg))))
                        (kill-local-variable 'selectrum-refine-candidates-function)
                        (let* ((default-directory dir)
                               (cs (split-string
                                    (shell-command-to-string (grep-expand-template command input)) "\n")))
                          `((candidates . ,(funcall prop cs))
                            (input . ,input)))))))
           (cand (let ((selectrum-should-sort-p nil)
                       (selectrum-minibuffer-bindings
                        (append
                         selectrum-minibuffer-bindings
                         `(("C-c C-o" . ,(selectrum-make-action (c)
                                                                ;; use rg.el to show the results in Occur buffer
                                                                (require 'rg)
                                                                (require 'compile)
                                                                ;; jump to current candidate in the *rg* buffer.
                                                                ;; rg implemented with `compile', so I make it work like below.
                                                                ;; let-bound method not working, unkown reason.
                                                                (let ((old-compilation-finish-functions compilation-finish-functions))
                                                                  (setq compilation-finish-functions
                                                                        (list
                                                                         (lambda (_a _b)
                                                                           (unwind-protect
                                                                               (progn
                                                                                 (pop-to-buffer (current-buffer))
                                                                                 (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" c)
                                                                                   (let ((file-name (match-string-no-properties 1 c))
                                                                                         (line-number (match-string-no-properties 2 c)))
                                                                                     (if rg-group-result
                                                                                         (progn
                                                                                           (re-search-forward (format "^File: %s" file-name) nil t)
                                                                                           (re-search-forward (format "^ *%s" line-number) nil t)
                                                                                           (re-search-forward input (point-at-eol) t))
                                                                                       (re-search-forward (format "%s:%s:" file-name line-number) nil t)
                                                                                       (re-search-forward input (point-at-eol) t)))))
                                                                             (setq compilation-finish-functions old-compilation-finish-functions)))))
                                                                  ;; dispatch to rg.el search.
                                                                  (cond ((eq type 'project) (rg-project input "*"))
                                                                        (t                  (rg input "*" dir))))))))))
                   (selectrum-read "rg: " cands
                                   :initial-input word
                                   :may-modify-candidates t
                                   :history 'selectrum-search-rg-history
                                   :require-match t))))
      (if (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
          (let ((file-name (match-string-no-properties 1 cand))
                (line-number (match-string-no-properties 2 cand)))
            (xref-push-marker-stack) ; use M-, to go back!
            (find-file (expand-file-name file-name dir))
            (goto-char (point-min))
            (forward-line (1- (string-to-number line-number)))
            (re-search-forward input (point-at-eol) t)
            (recenter))
        (message "Bad candidate?"))))


  (setq selectrum-num-candidates-displayed 12)

  (defun sel/recentf ()
    "Use `completing-read' to open a recent file."
    (interactive)
    (let ((files (mapcar 'abbreviate-file-name recentf-list)))
      (find-file (completing-read "Find recent file: " files nil t))))

  (defvar selectrum-swiper-history nil "Submission history for `selectrum-swiper'.")

  (defun sel/swiper ()
    "Search for a matching line and jump to the beginning of its text.  Obeys narrowing."
    (interactive)
    (let* ((selectrum-should-sort-p nil)
           (line-choices (cl-loop
                          with minimum-line-number = (line-number-at-pos (point-min) t)
                          with buffer-text-lines = (split-string (buffer-string) "\n")
                          with number-format = (format "%%0%dd: " (length (number-to-string (length buffer-text-lines))))
                          for txt in buffer-text-lines
                          for num from minimum-line-number to (+ minimum-line-number
                                                                 (1- (length buffer-text-lines)))
                          unless (string-empty-p txt) ; Just skip empty lines.
                          collect (concat (format number-format num) txt)))
           ;; Get the matching line.
           (chosen-line (completing-read "Jump to matching line: " line-choices
                                         nil t nil 'selectrum-swiper-history))
           ;; Stop at the ":". It is followed by one " ".
           (line-number-prefix (seq-take-while (lambda (char)
                                                 (not (char-equal ?: char)))
                                               chosen-line))
           ;; Get the corresponding line number, skipping the "L" in line-number-prefix.
           (chosen-line-number (string-to-number (substring line-number-prefix 1)))
           ;; Get the current line number for determining the travel distance.
           (current-line-number (line-number-at-pos (point) t)))

      (push-mark (point) t)
      ;; Manually edit history to remove line numbers.
      (setcar selectrum-swiper-history (substring chosen-line
                                                  ;; Want after line-prefix followed by ": ".
                                                  (+ (length line-number-prefix) 2)))
      (forward-line (- chosen-line-number current-line-number))
      (beginning-of-line-text 1)))

  ;; This is a command to replace the default yank-pop. It lets you choose in the kill-ring
  ;; Ref: https://www.gnu.org/software/emacs/manual/html_node/eintr/yank.html
  (defun sel/yank-pop (&optional arg)
    "Paste a previously killed string.
With just \\[universal-argument] as ARG, put point at beginning,
and mark at end.  Otherwise, put point at the end, and mark at
the beginning without activating it.

This is like `yank-pop'.  The differences are:

- This let you manually choose a candidate to paste.

- This doesn't delete the text just pasted if the previous
  command is `yank'."
    (interactive "P")
    (let* ((selectrum-should-sort-p nil)
           (text nil))
      (setq text
            (completing-read "Yank: "
                             (cl-remove-duplicates
                              kill-ring :test #'equal :from-end t)
                             nil 'require-match))
      (unless (eq last-command 'yank)
        (push-mark))
      (setq last-command 'yank)
      (setq yank-window-start (window-start))
      (when (and delete-selection-mode (use-region-p))
        (delete-region (region-beginning) (region-end)))
      (insert-for-yank text)
      (if (consp arg)
          (goto-char (prog1 (mark t)
                       (set-marker (mark-marker) (point) (current-buffer)))))))

  (defvar selectrum--toggle-project-data+ nil)

  (push (cons "C-," 'selectrum-toggle-project-file-scope+)
        selectrum-minibuffer-bindings)

  (defun selectrum-toggle-project-file-scope+ ()
    "Toggle to project scope when reading file names.
Depends on `projectile'."
    (interactive)
    (unless minibuffer-completing-file-name
      (user-error "Not reading file names"))
    (require 'projectile)
    (setq selectrum--previous-input-string nil)
    (cond ((and selectrum--toggle-project-data+
                (string-match "in project: \\'"
                              (buffer-substring
                               (point-min) (minibuffer-prompt-end))))
           (let ((inhibit-read-only t))
             (save-excursion
               (goto-char (minibuffer-prompt-end))
               (search-backward " in project")
               (delete-region (match-beginning 0)
                              (match-end 0)))
             (delete-minibuffer-contents))
           (insert (car selectrum--toggle-project-data+))
           (setq selectrum--preprocessed-candidates
                 (cdr selectrum--toggle-project-data+))
           (setq selectrum--toggle-project-data+ nil))
          (t
           (if-let ((input (selectrum-get-current-input))
                    (project (projectile-project-root
                              (file-name-directory input))))
               (let* ((inhibit-read-only t)
                      (ematch (file-name-nondirectory input))
                      (cands
                       (mapcar
                        (lambda (i)
                          (add-text-properties
                           0 (length i)
                           `(selectrum-candidate-full
                             ,(concat project i))
                           i)
                          i)
                        (projectile-project-files project))))
                 (save-excursion
                   (goto-char (minibuffer-prompt-end))
                   (search-backward ":")
                   (insert
                    (apply #'propertize
                           " in project"
                           (text-properties-at (point)))))
                 (setq selectrum--toggle-project-data+
                       (cons
                        input
                        selectrum--preprocessed-candidates))
                 (delete-minibuffer-contents)
                 (insert
                  (concat (abbreviate-file-name project) ematch))
                 (setq selectrum--preprocessed-candidates
                       (lambda (input)
                         (let ((ematch (file-name-nondirectory input)))
                           `((input . ,ematch)
                             (candidates . ,cands))))))
             (user-error "Not in project")))))
  )

(defvar selectrum-imenu+ nil)

(defun sel/imenu ()
  "Choose from `imenu' just like `counsel-imenu'."
  (interactive)
  (require 'imenu)
  (let* ((selectrum-should-sort-p nil)
         (candidates (let* ((imenu-auto-rescan t)
                            (items (imenu--make-index-alist t)))
                       ;; remove *Rescan*
                       (setq items (delete (assoc "*Rescan*" items) items))
                       ;; special mode
                       (when (eq major-mode 'emacs-lisp-mode)
                         (let ((fns (cl-remove-if #'listp items :key #'cdr)))
                           (if fns (setq items (nconc (cl-remove-if #'nlistp items :key #'cdr) `(("Functions" ,@fns)))))))
                       ;; refine
                       (cl-labels ((get-candidates (alist &optional prefix)
                                     (cl-mapcan
                                      (lambda (elm)
                                        (if (imenu--subalist-p elm)
                                            (get-candidates
                                             (cl-loop for (e . v) in (cdr elm)
                                                   collect (cons e (if (integerp v) (copy-marker v) v)))
                                             (concat prefix (if prefix ".") (car elm)))
                                          (let ((key (concat (if prefix (concat (propertize prefix 'face 'font-lock-keyword-face) ": "))
                                                             (car elm))))
                                            (list (cons key (cons key (if (overlayp (cdr elm)) (overlay-start (cdr elm)) (cdr elm))))))))
                                      alist)))
                         (setq items (get-candidates items)))
                       ;; sort
                       (cl-sort items #'string< :key #'car)))
         (cand (completing-read "Imenu: " (mapcar #'car candidates) nil t nil selectrum-imenu+)))
    (imenu (cdr (cl-find cand candidates :test #'string= :key #'car)))))

;; Jumping to an Info Node

(defvar Info-directory-list)
(defvar Info-additional-directory-list)
(defvar Info-default-directory-list)
(declare-function info-initialize "info")

(defcustom selectrum-info-default-other-window t
  "Whether `selectrum-info' (and derived commands) should display
the Info buffer in the other window by default. Use a prefix argument to
do the opposite."
  :type 'boolean
  :group 'selectrum)

(defun selectrum--info-get-child-node (top-node)
  "Create and select from a list of Info nodes found in the parent node."
  (let (;; It's reasonable to assume that sections are intentionally
        ;; ordered in a certain way, so we preserve that order.
        (selectrum-should-sort-p nil)
        ;; Headers look like "* Some Thing::      Description",
        ;; where descriptions are optional and might continue on
        ;; the next line.
        (sub-topic-format (rx "* "
                              (group (+? (not ?:)))
                              "::"
                              ;; Include the description, if one exists.
                              ;; If it doesn't, the line ends immediately.
                              (or "\n"
                                  (seq
                                   (0+ blank)
                                   (group (+? anychar))
                                   ;; Sometimes a heading follows on the next line,
                                   ;; and sometimes there's any empty blank line
                                   ;; (such as before a section title).  For now,
                                   ;; assume continuation lines use indentation and
                                   ;; other lines don't.
                                   ;; (or "\n\n" "\n*")
                                  "\n" (not blank))))))
    (save-match-data
      (save-selected-window
        (completing-read
         "Info Sub-Topic: "
         (with-temp-buffer
           ;; Some nodes created from multiple files.
           (info top-node (current-buffer))
           (goto-char (point-min))
           (cl-loop
            while (re-search-forward sub-topic-format nil t)
            do (forward-line 0)         ; Go back to start of line.
            collect (propertize (match-string 1)
                                'selectrum-candidate-display-suffix
                                ;; Include the description, if one exists.
                                (when-let ((desc (match-string 2)))
                                  (propertize
                                   (concat " - "
                                           (replace-regexp-in-string
                                            "\n" ""
                                            (replace-regexp-in-string
                                             " +" " " desc)))
                                   'face 'completions-annotations))))))))))

;;;###autoload
(defun selectrum-info (other-window-opposite-p &optional top-node)
    "Go to a node of an Info topic.  With a prefix argument, do the opposite
of `selectrum-info-default-other-window'.
For example, you can go to \"(magit)Notes\" by selecting \"magit\", then \"Notes\" ."
  (interactive "P")

  ;; Initialize Info information so that the proper directories
  ;; can be found.
  (info-initialize)

  (save-match-data
    (let* ((use-other-window (if other-window-opposite-p
                                 (not selectrum-info-default-other-window)
                               selectrum-info-default-other-window))
           ;; Get all Info files.
           (node-files
            (cl-loop for directory in (append (or Info-directory-list
                                                  Info-default-directory-list)
                                              Info-additional-directory-list)
                     ;; If the directory exists
                     when (file-directory-p directory)
                     ;; get all files with ".info" in their name.
                     append (directory-files directory nil "\\.info" t)))

           ;; Get the names of the Info nodes, based on the file names.
           (node-names (cl-remove-duplicates
                        (cl-loop for file in node-files
                                 do (string-match "\\(.+?\\)\\." file)
                                 collect (match-string 1 file))
                        :test #'equal))

           ;; Select a top node/topic.
           (chosen-top-node (cond
                             ((null top-node)
                              (completing-read "Info Topic: " node-names nil t))
                             ((member top-node node-names)
                              top-node)
                             (t (error "Top-level Info node does not exist: %s"
                                       top-node))))

           ;; Select a child node.
           (chosen-child-node (selectrum--info-get-child-node chosen-top-node)))

      ;; Go to the chosen child node.
      (funcall (if use-other-window
                   #'info-other-window
                 #'info)
               (format "(%s)%s" chosen-top-node chosen-child-node)))))

;;;###autoload
(defun selectrum-info-elisp-manual (other-window-opposite-p)
  "Like `selectrum-info', but directly choose nodes from the Emacs Lisp (Elisp) manual."
  (interactive "P")
  (selectrum-info other-window-opposite-p "elisp"))

;;;###autoload
(defun selectrum-info-emacs-manual (other-window-opposite-p)
  "Like `selectrum-info', but directly choose nodes from the Emacs manual."
  (interactive "P")
  (selectrum-info other-window-opposite-p "emacs"))

;;;###autoload
(defun selectrum-info-org-manual (other-window-opposite-p)
  "Like `selectrum-info', but directly choose nodes from the Org manual."
  (interactive "P")
  (selectrum-info other-window-opposite-p "org"))

(use-package ctrlf
  :defer .5)

(provide 'env-selectrum)
