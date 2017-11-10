(setq telephone-line-primary-left-separator 'telephone-line-identity-left)
(setq telephone-line-primary-right-separator 'telephone-line-identity-right)
;; (setq telephone-line-primary-left-separator 'telephone-line-halfsin-left)
;; (setq telephone-line-primary-right-separator 'telephone-line-halfsin-right)

;; Set mode-line height
(setq telephone-line-height 28)


(set-face-bold 'mode-line nil)
;; (set-face-attribute 'mode-line-inactive nil (:box (:line-width 1)))

;; Left edge
;; TODO: gray background for buffer and mode segment in inactive line
(setq telephone-line-lhs
      '(
        (accent . (my-evil-segment))
        (nil    . (my-modified-status-segment))
        (nil    . (my-read-only-status-segment))))
        ;; (nil . (my-flycheck-segment))))

(setq telephone-line-center-lhs
      ;; '((nil . (my-major-mode-segment)))
      '(
        (nil)
        (accent . (my-flycheck-segment
                   my-major-mode-segment))))

(setq telephone-line-center-rhs
      '(
        (accent . (my-buffer-segment
                   my-position-segment))
        (nil)))
        ;; (nil . (my-vc-segment))))

;; Right edge
(setq telephone-line-rhs
      '(
        (nil)
        (accent . ((my-coding-segment :active)))))
        ;; (nil . (my-vc-segment))
        ;; (nil . (my-position-segment))
        ;; (nil . (my-major-mode-segment))
        ;; (accent . ((my-coding-segment :active)))))

(provide 'modeline-flat)
