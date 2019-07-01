(use-package calfw
  :defer t
  :commands (cfw:open-calendar-buffer))

(defalias 'cal 'cfw:open-calendar-buffer)

(provide 'use-calendar)
