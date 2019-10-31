(use-package kotlin-mode)

(use-package flycheck-kotlin
  :after kotlin-mode
  :hook  ((kotlin-mode . flycheck-mode)
          (kotlin-mode . flycheck-kotlin-setup)))


(use-package gradle-mode
  :hook (kotlin-mode . gradle-mode)
  :config
  (setq gradle-use-gradlew t)
  (setq gradle-gradlew-executable "./gradlew"))

(provide 'use-kotlin)
