;;  -*- lexical-binding: t; -*-
(use-package ediff
  :defer t
  :commands (ediff))

(use-package evil-ediff
  :if (featurep 'evil)
  :after ediff)

(provide 'use-diff)
