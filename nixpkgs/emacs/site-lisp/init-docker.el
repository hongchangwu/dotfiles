;;; init-docker.el --- init file for Docker

;;; Commentary:

;; Dockerfile mode with LSP support.

;;; Code:

(use-package dockerfile-mode
  :mode "\\.*Dockerfile$"
  :hook (dockerfile-mode . lsp))

(provide 'init-docker)

;;; init-docker.el ends here
