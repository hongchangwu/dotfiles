;;; init-octave.el --- init file for Octave

;;; Commentary:

;; Octave mode has been built-in since Emacs 21.

;;; Code:

(use-package octave
  :init
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode)))

(provide 'init-octave)

;;; init-octave.el ends here
