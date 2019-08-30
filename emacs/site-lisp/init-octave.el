;; Octave

(use-package octave
  :init
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode)))

(provide 'init-octave)
