;; Semantic

(use-package semantic
  :hook
  ((c-mode c++-mode java-mode) . semantic-mode))

(provide 'init-semantic)
