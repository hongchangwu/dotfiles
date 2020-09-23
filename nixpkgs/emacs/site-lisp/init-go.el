;;; init-go.el --- init file for Go

;;; Commentary:

;; Turn on LSP and Flycheck.

;;; Code:

(use-package go-mode
  :ensure-system-package
  (gopls. "go get golang.org/x/tools/gopls@latest")
  :hook
  (go-mode . lsp)
  (go-mode . flycheck-mode))

(provide 'init-go)

;;; init-go.el ends here
