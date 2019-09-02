(use-package eshell-git-prompt
  :hook
  (eshell-mode . (lambda () (setq show-trailing-whitespace nil)))
  :config
  (setenv "PAGER" "/bin/cat")
  (eshell-git-prompt-use-theme 'powerline))

(provide 'init-eshell)
