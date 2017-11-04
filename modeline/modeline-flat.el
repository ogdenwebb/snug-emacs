(setq telephone-line-primary-left-separator 'telephone-line-flat)
(setq telephone-line-primary-right-separator 'telephone-line-flat)

;; Set mode-line height
(setq telephone-line-height 24)

;; Left edge
;; TODO: gray background for buffer and mode segment in inactive line
(setq telephone-line-lhs
      '((nil   . (my-evil-segment))
        (nil    . (my-buffer-segment))
        (nil    . (my-modified-status-segment))
        (nil    . (my-read-only-status-segment))))
        ;; (nil    . (my-flycheck-segment))))

;; Right edge
(setq telephone-line-rhs
      '((nil . (my-vc-segment))
        (nil . (my-position-segment))
        (nil . (my-major-mode-segment))
        (nil . ((my-coding-segment :active)))))

(provide 'modeline-flat)
