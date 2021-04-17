;;; init-tree-sitter.el --- init file for tree-sitter

;;; Commentary:

;;; Code:

(use-package tree-sitter
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs)

(provide 'init-tree-sitter)

;;; init-tree-sitter.el ends here
