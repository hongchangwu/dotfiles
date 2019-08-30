;; Elixir

(use-package elixir-mode)
(use-package alchemist
  :after elixir-mode
  :hook
  (elixir-mode . alchemist-mode)
  :custom
  (alchemist-hooks-test-on-save t))

(provide 'init-elixir)
