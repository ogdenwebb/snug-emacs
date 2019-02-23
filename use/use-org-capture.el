;; Org protocol setup
;; Requirement: https://github.com/sprig/org-capture-extension
(use-package org-protocol
  :config
  (setq org-capture-templates `(
  ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
        "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
  ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
        "* %? [[%:link][%:description]] \nCaptured On: %U")
  ))
  )

(provide 'use-org-capture)
