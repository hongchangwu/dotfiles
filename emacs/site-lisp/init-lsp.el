;;; init-lsp.el --- init file for LSP

;;; Commentary:

;; I chose lsp-mode over eglot because it seems to be more actively developed.

;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-register-client)
  :init (setq lsp-keymap-prefix "C-c l")
  :bind
  ("<f2>" . lsp-rename)
  :config
  (setq lsp-prefer-flymake nil))

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

;;; init-lsp.el ends here
