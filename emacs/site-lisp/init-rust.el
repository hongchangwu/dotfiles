(use-package rust-mode
  :ensure-system-package
  ((rustup . "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh")
   (rls . "rustup component add rls rust-analysis rust-src"))
  :hook
  (rust-mode . lsp)
  (rust-mode . flycheck-mode)
  :bind
  (:map rust-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :config
  (require 'lsp-rust))

(use-package cargo
  :ensure-system-package
  (rustfmt . "rustup component add rustfmt")
  :hook
  (rust-mode . cargo-minor-mode))

(provide 'init-rust)
