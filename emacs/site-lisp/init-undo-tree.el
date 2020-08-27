;;; init-undo-tree.el --- init file for Undo Tree

;;; Commentary:

;; Visual undo system.

;;; Code:

(use-package undo-tree
  :delight undo-tree-mode
  :config
  (global-undo-tree-mode 1))

(provide 'init-undo-tree)

;;; init-undo-tree.el ends here
