;; Clojure mode

(use-package clojure-mode
  :hook
  (((clojure-mode nrepl-mode) . paredit-mode)
   (cider-mode . cider-turn-on-eldoc-mode)))

(use-package clojure-mode-extra-font-locking
  :custom
  (nrepl-log-messages t)
  (nrepl-hide-special-buffers t)
  (cider-show-error-buffer 'except-in-repl))

(provide 'init-clojure)
