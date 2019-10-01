(use-package rust-mode
  :ensure-system-package
  ((rustup . "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh")
   (rls . "rustup component add rls rust-analysis rust-src rustfmt clippy"))
  :hook
  (rust-mode . lsp)
  (rust-mode . flycheck-mode)
  :bind
  (:map rust-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :config
  (require 'lsp-rust))

(use-package cargo
  :hook
  (rust-mode . cargo-minor-mode))

(provide 'init-rust)
