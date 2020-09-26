;;; init-avy.el --- init file for avy

;;; Commentary:

;; Use `C-:` to jump to any position by entering 1 character and
;; use `C-'` to jump to any position by entering 2 characters.

;;; Code:

(use-package avy
  :bind
  ("C-\"" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line))

(provide 'init-avy)

;;; init-avy.el ends here
