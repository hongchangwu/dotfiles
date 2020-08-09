;;; init-company.el --- init file for Elixir

;;; Commentary:

;; Install `elixir-mode` and `alchemist`.

;;; Code:

;; Elixir
(use-package elixir-mode)
(use-package alchemist
  :after elixir-mode
  :hook
  (elixir-mode . alchemist-mode)
  :custom
  (alchemist-hooks-test-on-save t))

(provide 'init-elixir)

;;; init-elixir.el ends here
