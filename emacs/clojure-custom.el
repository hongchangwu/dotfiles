;; Clojure mode
(require 'clojure-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(require 'clojure-mode-extra-font-locking)
(setq nrepl-log-messages t)
(setq nrepl-hide-special-buffers t)
(setq cider-show-error-buffer 'except-in-repl)
