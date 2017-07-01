;; Elixir
(require 'elixir-mode)
(require 'alchemist)
(add-hook 'elixir-mode-hook 'alchemist-mode)
(setq alchemist-hooks-test-on-save t)
