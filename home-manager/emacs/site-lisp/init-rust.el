;;; init-rust.el --- init file for Rust

;;; Commentary:

;; Use the official Rust mode with RLS.

;;; Code:

(use-package rust-mode
  :hook
  (rust-mode . lsp)
  (rust-mode . flycheck-mode)
  :bind
  (:map rust-mode-map
        ("<tab>" . company-indent-or-complete-common)))

(use-package cargo
  :hook
  (rust-mode . cargo-minor-mode))

(provide 'init-rust)

;;; init-rust.el ends here
