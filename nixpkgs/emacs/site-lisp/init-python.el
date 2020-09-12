;;; init-python.el --- init file for Python

;;; Commentary:

;; Enable `pyls` for language server support.

;;; Code:

(use-package python-mode
  :hook
  (python-mode . (lambda () (flymake-mode -1)))
  (python-mode . flycheck-mode)
  (python-mode . lsp))

(provide 'init-python)

;;; init-python.el ends here
