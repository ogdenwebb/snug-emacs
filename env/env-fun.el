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
;; TODO: ignore hl-line-mode or temp disable it
;;;###autoload
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; Skip prompt in `find-file-at-point' for single cancidate.
(with-eval-after-load 'ffap
  (defun ffap-prompter (&optional guess)
    (let ((filename (ffap-guess-file-name-at-point)))
      (if (and (stringp filename)
               (not (file-directory-p filename))
               (file-exists-p filename))
          filename

        ;; Does guess and prompt step for find-file-at-point.
        ;; Extra complication for the temporary highlighting.
        (unwind-protect
            ;; This catch will let ffap-alist entries do their own prompting
            ;; and then maybe skip over this prompt (ff-paths, for example).
            (catch 'ffap-prompter
              (ffap-read-file-or-url
               (if ffap-url-regexp "Find file or URL: " "Find file: ")
               (prog1
                   (let ((mark-active nil))
                     ;; Don't use the region here, since it can be something
                     ;; completely unwieldy.  If the user wants that, she could
                     ;; use M-w before and then C-y.  --Stef
                     (setq guess (or guess (ffap-guesser)))) ; using ffap-alist here
                 (and guess (ffap-highlight))
                 )))
          (ffap-highlight t))))))

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

;; (use-package smart-hungry-delete
;;   :if (>= emacs-major-version 25)
;;   :bind (:map prog-mode-map
;;          ("<backspace>" .
;;           smart-hungry-delete-backward-char)
;;          ("C-d" .
;;           smart-hungry-delete-forward-char))
;;   :hook ((prog-mode .
;;           smart-hungry-delete-default-prog-mode-hook)
;;          (c-mode-common .
;;           smart-hungry-delete-default-c-mode-common-hook)
;;          (python-mode .
;;           smart-hungry-delete-default-c-mode-common-hook)
;;          (text-mode .
;;           smart-hungry-delete-default-text-mode-hook)))

;; TODO: replace with smart-hungry-delete
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

;;;###autoload
(defun snug/split-and-focus-horizontally ()
   "Split window below and focus."
    (interactive)
    (split-window-below)
    (other-window 1))

;;;###autoload
(defun snug/split-and-focus-verically ()
   "Split window right and focus."
    (interactive)
    (split-window-right)
    (other-window 1))

;;;###autoload
(defun snug/delete-window-or-kill-buffer ()
  "TODO"
  (interactive)
  (if (one-window-p)
      (kill-current-buffer)
    (delete-window)))

;;;###autoload
(defun snug/delete-window-or-previous-buffer ()
  "TODO"
  (interactive)
  (if (one-window-p)
      (previous-buffer)
    (delete-window)))

;;;###autoload
(defun snug/find-next-file (&optional backward)
  "Find the next file (by name) in the current directory.

With prefix arg, find the previous file."
  (interactive "P")
  (when buffer-file-name
    (let* ((file (expand-file-name buffer-file-name))
           (files (cl-remove-if (lambda (file) (cl-first (file-attributes file)))
                                (sort (directory-files (file-name-directory file) t nil t) 'string<)))
           (pos (mod (+ (cl-position file files :test 'equal) (if backward -1 1))
                     (length files))))
      (find-file (nth pos files)))))


(provide 'env-fun)
