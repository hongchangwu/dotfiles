;;; init-eldoc --- init file for Eldoc

;;; Commentary:

;; Turn on `eldoc-mode` for Lisp modes.

;;; Code:

(use-package eldoc
  :hook
  ((emacs-lisp-mode lisp-interaction-mode ielm-mode) . turn-on-eldoc-mode))

(provide 'init-eldoc)

;;; init-eldoc.el ends here
