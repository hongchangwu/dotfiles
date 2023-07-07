;;; init-yasnippet.el --- init file for Yasnippet

;;; Commentary:

;; Yet another template system for Emacs.

;;; Code:

(use-package yasnippet
  :delight yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
