(use-package lsp-mode
  :commands (lsp lsp-register-client)
  :bind ("<f2>" . lsp-rename))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)))

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
