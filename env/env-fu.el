;; Module with useful custom functions -*- lexical-binding: t; -*-

;;;###autoload
(defun elmax/newline-and-indent ()
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

;; Disable prompt in find file at point
;; TODO: add to evil jump list

;;;###autoload
(defun ffap-read-file-or-url (prompt guess)
  "Read file or URL from minibuffer, with PROMPT and initial GUESS."
  (or guess (setq guess default-directory))
  (let (dir)
    ;; Tricky: guess may have or be a local directory, like "w3/w3.elc"
    ;; or "w3/" or "../el/ffap.el" or "../../../"
    (unless (ffap-url-p guess)
      (unless (ffap-file-remote-p guess)
        (setq guess
              (abbreviate-file-name (expand-file-name guess))))
      (setq dir (file-name-directory guess)))
    (let ((minibuffer-completing-file-name t)
          (completion-ignore-case read-file-name-completion-ignore-case)
          (fnh-elem (cons ffap-url-regexp 'url-file-handler)))
      ;; Explain to `rfn-eshadow' that we can use URLs here.
      (unwind-protect
          (push fnh-elem file-name-handler-alist)
        (setq guess (abbreviate-file-name (expand-file-name guess))
              dir (file-name-directory guess)))
      ;; Remove the special handler manually.  We used to just let-bind
      ;; file-name-handler-alist to preserve its value, but that caused
      ;; other modifications to be lost (e.g. when Tramp gets loaded
      ;; during the completing-read call).
      (setq file-name-handler-alist (delq fnh-elem file-name-handler-alist))))
  (or (ffap-url-p guess)
      (substitute-in-file-name guess)))

;;;###autoload
(defun line-length (n)
  "Length of the Nth line."
  (save-excursion
    (goto-char (point-min))
    (if (zerop (forward-line (1- n)))
        (- (line-end-position)
           (line-beginning-position)))))

(defun elmax/empty-line? ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

;; TODO: indent based languanges like Python
(defun elmax/smart-backspace ()
  (interactive)
  (if (elmax/empty-line?)
      (if (= (line-length (line-number-at-pos)) 0)
          (delete-indentation)
        (delete-horizontal-space))
    (backward-delete-char-untabify 1 nil)))

;; TODO: Jump to defenition
;; https://github.com/syl20bnr/spacemacs/blob/master/core/core-jump.el

;; (defun xdg-open-from-kill-ring ()
;;   "Launch the default xdg appplication."
;;   (interactive)
;;   (shell-command (concat "xdg-open " (substring-no-properties (car kill-ring)))))
;; ;;change xdg-open to whatever launcher you have (xdg-open is the default on most linux systems)

;;;###autoload
(defun align-repeat (start end regexp &optional justify-right after)
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
(defun elmax/evil-select-pasted ()
  (interactive)
  (let ((start-marker (evil-get-marker ?\[))
        (end-marker (evil-get-marker ?\])))
    (evil-visual-select start-marker end-marker)))

;;;###autoload
(defun column-number-at-pos (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))


(provide 'env-fu)
