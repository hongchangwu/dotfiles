(use-package rust-mode
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
  :after rust-mode
  :hook
  (rust-mode . cargo-minor-mode))

(use-package racer
  :after (eldoc rust-mode)
  :hook
  (racer-mode . eldoc-mode)  
  (rust-mode . racer-mode))

(use-package flyecheck-rust
  :after (flycheck rust-mode)
  :hook
  (flycheck-mode . flycheck-rust-setup))

(provide 'init-rust)
