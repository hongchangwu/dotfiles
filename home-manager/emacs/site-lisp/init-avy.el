;;; init-avy.el --- init file for avy

;;; Commentary:

;; Use `C-c SPC` to jump to any position by entering 1 character and
;; use `C-c C-SPC` to jump to any position by entering 2 characters.

;;; Code:

(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char)
  ("C-c C-SPC" . avy-goto-char-2)
  ("M-g f" . avy-goto-line))

(provide 'init-avy)

;;; init-avy.el ends here
