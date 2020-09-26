;;; init-tree-sitter.el --- init file for tree-sitter

;;; Commentary:

;;; Code:

(use-package tree-sitter
  :straight (:host github
             :repo "ubolonton/emacs-tree-sitter"
             :branch "master"
             :files ("lisp/*.el"))
  :config
  (global-tree-sitter-mode)
  ;; (add-hook 'rust-mode-hook #'tree-sitter-hl-mode)
  )

(use-package tree-sitter-langs
  :straight (:host github
             :repo "ubolonton/emacs-tree-sitter"
             :branch "master"
             :files ("langs/*.el" "langs/queries")))

(provide 'init-tree-sitter)

;;; init-tree-sitter.el ends here
