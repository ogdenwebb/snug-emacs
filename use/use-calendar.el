;; Calendar module -*- lexical-binding: t; -*-

(use-package calendar
  :straight nil
  :hook (calendar-today-visible . calendar-mark-today)
  :config
  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (concat " (" time-zone ")")))
        calendar-week-start-day 1 ; Monday
        calendar-date-style 'iso
        ;; calendar-mark-diary-entries-flag t
        calendar-holidays
        (append holiday-general-holidays holiday-local-holidays
                holiday-other-holidays holiday-christian-holidays
                holiday-islamic-holidays holiday-oriental-holidays
                holiday-solar-holidays))
  )


;; Calfw - A calendar framework for Emacs
(use-package calfw
  :defer t
  :preface
  (defalias 'cal 'cfw:open-calendar-buffer)
  :commands (cfw:open-calendar-buffer))


(provide 'use-calendar)
