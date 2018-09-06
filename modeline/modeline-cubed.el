;; Set separator style
(setq telephone-line-primary-left-separator 'telephone-line-halfsin-left)
(setq telephone-line-primary-right-separator 'telephone-line-halfsin-right)

;; Set mode-line height
(setq telephone-line-height 26)

;; Left edge
;; TODO: gray background for buffer and mode segment in inactive line
(setq telephone-line-lhs
      '((evil   . (my-evil-segment-icons))
        (nil    . (telephone-line-projectile-buffer-segment))
        (nil    . (my-modified-status-segment))
        (nil    . (my-read-only-status-segment))
        (nil    . (selection-info))))
        ;; (nil    . (my-flycheck-segment))))

;; Right edge
(setq telephone-line-rhs
      '((nil    . (my-vc-segment))
        (accent . (my-position-segment))
        (nil    . (my-major-mode-segment-icon))
        (accent . ((my-coding-segment :active)))))

(provide 'modeline-cubed)
