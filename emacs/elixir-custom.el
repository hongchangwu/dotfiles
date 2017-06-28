;; Elixir
(require 'elixir-mode)
(require 'alchemist-mode)
(add-hook 'elixir-mode-hook 'alchemist-mode)
(setq alchemist-hooks-test-on-save t)
