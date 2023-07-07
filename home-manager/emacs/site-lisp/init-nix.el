;;; init-nix.el --- init file for Nix

;;; Commentary:

;; Basic syntax highlighting and indentation for .nix files.

;;; Code:

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook
  (nix-mode . lsp))

(provide 'init-nix)

;;; init-nix.el ends here
