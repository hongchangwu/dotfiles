(use-package go-mode
  :hook
  (go-mode . lsp)
  (go-mode . flycheck-mode))

(provide 'init-go)
