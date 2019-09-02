(use-package rust-mode
  :ensure-system-package
  (rustup . "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh")
  :after company
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  :hook
  (rust-mode . company-mode)
  :bind
  (:map rust-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-tooltip-align-annotations t))

(use-package cargo
  :ensure-system-package
  (rustfmt . "rustup component add rustfmt")
  :after rust-mode
  :hook
  (rust-mode . cargo-minor-mode))

(use-package racer
  :ensure-system-package
  (racer
   .
   "rustup toolchain add nightly && rustup component add rust-src && cargo +nightly install racer")
  :after (eldoc rust-mode)
  :hook
  (rust-mode . racer-mode)
  (racer-mode . eldoc-mode)
  (racer-help-mode . (lambda () (setq show-trailing-whitespace nil)))
  :bind
  (:map racer-mode-map
        ("C-c C-t" . racer-describe)))

(use-package flycheck-rust
  :after (flycheck rust-mode)
  :hook
  (flycheck-mode . flycheck-rust-setup))

(provide 'init-rust)
