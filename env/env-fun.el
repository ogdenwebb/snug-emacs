;; Module with useful custom functions -*- lexical-binding: t; -*-

;;;###autoload
(defun snug/newline-and-indent ()
  "Inserts a newline and possibly indents it. Also continues comments if
executed from a commented line"
  (interactive)
  (cond ((sp-point-in-string)
         (newline))
        ((and (sp-point-in-comment)
              comment-line-break-function)
         (funcall comment-line-break-function))
        (t
         (newline nil t)
         (indent-according-to-mode))))

;; Rename file and buffer
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
;;;###autoload
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; Display face under cursor
;;;###autoload
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; Skip prompt in find file at point for single cancidate.
;;;###autoload
;; (with-eval-after-load 'ffap
;;   ;; (defun find-single-match ()
;;   ;;   (if ; (= (length (ffap-guesser)) 1)
;;   ;;       (stringp (ffap-guesser))
;;   ;;          (find-file (ffap-guesser))))

;;   ;; (advice-add 'ffap-prompter :before #'find-single-match))

;; (defun ffap-prompter (&optional guess)
;;   ;; Does guess and prompt step for find-file-at-point.
;;   ;; Extra complication for the temporary highlighting.
;;   (if (stringp (ffap-guesser))
;;       (find-file (ffap-guesser))
;;   (unwind-protect
;;       ;; This catch will let ffap-alist entries do their own prompting
;;       ;; and then maybe skip over this prompt (ff-paths, for example).
;;       (catch 'ffap-prompter
;;         (ffap-read-file-or-url
;;          (if ffap-url-regexp "Find file or URL: " "Find file: ")
;;          (prog1
;;              (let ((mark-active nil))
;;                ;; Don't use the region here, since it can be something
;;                ;; completely unwieldy.  If the user wants that, she could
;;                ;; use M-w before and then C-y.  --Stef
;;                (setq guess (or guess (ffap-guesser)))) ; using ffap-alist here
;;            (and guess (ffap-highlight))
;;            )))
;;     (ffap-highlight t)))))

;; Redefine find-file-at-point to disable prompt for single candidate
;; ;;;###autoload
;; (with-eval-after-load 'ffap
;;   (defun find-file-at-point (&optional filename)
;;     "Find FILENAME, guessing a default from text around point.
;; If `ffap-url-regexp' is not nil, the FILENAME may also be an URL.
;; With a prefix, this command behaves exactly like `ffap-file-finder'.
;; If `ffap-require-prefix' is set, the prefix meaning is reversed.
;; See also the variables `ffap-dired-wildcards', `ffap-newfile-prompt',
;; `ffap-url-unwrap-local', `ffap-url-unwrap-remote', and the functions
;; `ffap-file-at-point' and `ffap-url-at-point'."
;;     (interactive)
;;     (if (and (called-interactively-p 'interactive)
;;              (if ffap-require-prefix (not current-prefix-arg)
;;                current-prefix-arg))
;;         ;; Do exactly the ffap-file-finder command, even the prompting:
;;         (let (current-prefix-arg)		; we already interpreted it
;;           (call-interactively ffap-file-finder))
;;       (or filename
;;           (setq filename
;;                 (if (stringp (ffap-guesser))
;;                     (ffap-guesser)
;;                   (ffap-prompter)))
;;           (let ((url (ffap-url-p filename)))
;;             (cond
;;              (url
;;               (let (current-prefix-arg)
;;                 (funcall ffap-url-fetcher url)))
;;              ((and ffap-pass-wildcards-to-dired
;;                    ffap-dired-wildcards
;;                    (string-match ffap-dired-wildcards filename))
;;               (funcall ffap-directory-finder filename))
;;              ((and ffap-dired-wildcards
;;                    (string-match ffap-dired-wildcards filename)
;;                    find-file-wildcards
;;                    ;; Check if it's find-file that supports wildcards arg
;;                    (memq ffap-file-finder '(find-file find-alternate-file)))
;;               (funcall ffap-file-finder (expand-file-name filename) t))
;;              ((or (not ffap-newfile-prompt)
;;                   (file-exists-p filename)
;;                   (y-or-n-p "File does not exist, create buffer? "))
;;               (funcall ffap-file-finder
;;                        ;; expand-file-name fixes "~/~/.emacs" bug sent by CHUCKR.
;;                        (expand-file-name filename)))
;;              ;; User does not want to find a non-existent file:
;;              ((signal 'file-missing (list "Opening file buffer"
;;                                           "No such file or directory"
;;                                           filename)))))))))


;; (defun ffap-read-file-or-url (prompt guess)
;;   "Read file or URL from minibuffer, with PROMPT and initial GUESS."
;;   (or guess (setq guess default-directory))
;;   (let (dir)
;;     ;; Tricky: guess may have or be a local directory, like "w3/w3.elc"
;;     ;; or "w3/" or "../el/ffap.el" or "../../../"
;;     (unless (ffap-url-p guess)
;;       (unless (ffap-file-remote-p guess)
;;         (setq guess
;;               (abbreviate-file-name (expand-file-name guess))))
;;       (setq dir (file-name-directory guess)))
;;     (let ((minibuffer-completing-file-name t)
;;           (completion-ignore-case read-file-name-completion-ignore-case)
;;           (fnh-elem (cons ffap-url-regexp 'url-file-handler)))
;;       ;; Explain to `rfn-eshadow' that we can use URLs here.
;;       (unwind-protect
;;           (push fnh-elem file-name-handler-alist)
;;         (setq guess (abbreviate-file-name (expand-file-name guess))
;;               dir (file-name-directory guess)))
;;       ;; Remove the special handler manually.  We used to just let-bind
;;       ;; file-name-handler-alist to preserve its value, but that caused
;;       ;; other modifications to be lost (e.g. when Tramp gets loaded
;;       ;; during the completing-read call).
;;       (setq file-name-handler-alist (delq fnh-elem file-name-handler-alist))))
;;   (or (ffap-url-p guess)
;;       (substitute-in-file-name guess)))

;;;###autoload
(defun line-length (n)
  "Length of the Nth line."
  (save-excursion
    (goto-char (point-min))
    (if (zerop (forward-line (1- n)))
        (- (line-end-position)
           (line-beginning-position)))))

(defun snug/empty-line? ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

;; ;; TODO: indent based languanges like Python
;; (defun snug/smart-backspace ()
;;   (interactive)
;;   (if (snug/empty-line?)
;;       (if (= (line-length (line-number-at-pos)) 0)
;;           (delete-indentation)
;;         (delete-horizontal-space))
;;     (backward-delete-char-untabify 1 nil)))

(defun snug/smart-backspace ()
  (interactive)
  (if (string-match "^[[:space:]]*$" (thing-at-point 'line))
      (delete-indentation)
    (backward-delete-char-untabify 1)))

;; TODO: Jump to defenition
;; https://github.com/syl20bnr/spacemacs/blob/master/core/core-jump.el

;; (defun xdg-open-from-kill-ring ()
;;   "Launch the default xdg appplication."
;;   (interactive)
;;   (shell-command (concat "xdg-open " (substring-no-properties (car kill-ring)))))
;; ;;change xdg-open to whatever launcher you have (xdg-open is the default on most linux systems)

;;;###autoload
(defun snug/align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
  If JUSTIFY-RIGHT is non nil justify to the right instead of the
  left. If AFTER is non-nil, add whitespace to the left instead of
  the right."

  (interactive "r\nsAlign repeat regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))
    (message "complete-regexp: %S" complete-regexp)
    (align-regexp start end complete-regexp group 1 t)))

;;;###autoload
(defun snug/evil-select-pasted ()
  (interactive)
  (let ((start-marker (evil-get-marker ?\[))
        (end-marker (evil-get-marker ?\])))
    (evil-visual-select start-marker end-marker)))

;;;###autoload
(defun column-number-at-pos (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))

;;;###autoload
(defun snug/copy-whole-buffer ()
  "Copy whole buffer"
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (copy-region-as-kill 1 (buffer-size))))

;;;###autoload
(defun snug/format-date (format)
  (let ((system-time-locale "en_US.UTF-8"))
    (insert (format-time-string format))))

;;;###autoload
(defun snug/insert-date ()
  (interactive)
  (snug/format-date "%A, %B %d %Y"))

;;;###autoload
(defun snug/insert-date-and-time ()
  (interactive)
  (snug/format-date "%Y-%m-%d %H:%M:%S"))

(defun snug/insert-current-filename ()
  "Insert current buffer filename."
  (interactive)
  (insert (file-relative-name buffer-file-name)))

(provide 'env-fun)
