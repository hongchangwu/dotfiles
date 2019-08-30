;; Vim like word search

(use-package evil
  :bind
  ("C-*" . evil-search-word-forward)
  ("C-#" . evil-search-word-backward))

(provide 'init-evil)
