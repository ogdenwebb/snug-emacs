;; TODO
;; (use-package mpdel
;;   :commands (mpdel-mode mpd-playlist-open))

;; (use-package ivy-mpdel
;;   :commands (ivy-mpdel-list ivy-mpdel-artists ivy-mpdel-stored-playlists))

(use-package colorpicker
  :straight (:host github :repo "syohex/emacs-colorpicker" :files ("*.el" "*.py" "script"))
  :commands (colorpicker))

(provide 'use-tools)
