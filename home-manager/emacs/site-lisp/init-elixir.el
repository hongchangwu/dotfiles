;;; init-elixir.el --- init file for Elixir

;;; Commentary:

;; Use `elixir-mode' with LSP. `alchemist' is obsolete.

;;; Code:

(use-package elixir-mode
  :hook
  (elixir-mode . lsp)
  (elixir-mode . flycheck-mode))

(provide 'init-elixir)

;;; init-elixir.el ends here
