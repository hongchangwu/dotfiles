;;; init-python.el --- init file for Python

;;; Commentary:

;; Use Emacs' built-in Python mode with LSP.

;;; Code:

(use-package python
  :straight nil
  :hook
  (python-mode . (lambda () (flymake-mode -1)))
  (python-mode . flycheck-mode)
  (python-mode . lsp))

(provide 'init-python)

;;; init-python.el ends here
