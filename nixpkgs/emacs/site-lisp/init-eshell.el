;;; init-eshell.el --- init file for Eshell

;;; Commentary:

;; Set up fancy powerline theme for Eshell.

;;; Code:

(use-package eshell-git-prompt
  :hook
  (eshell-mode . (lambda () (setq show-trailing-whitespace nil)))
  :config
  (setenv "PAGER" "cat")
  (eshell-git-prompt-use-theme 'powerline))

(provide 'init-eshell)

;;; init-eshell.el ends here
