;;; init-lsp.el --- init file for LSP

;;; Commentary:

;; I chose lsp-mode over eglot because it seems to be more actively developed.

;;; Code:

(use-package lsp-mode
  ;; https://github.com/emacs-lsp/lsp-mode/pull/2109
  :straight (:host github
             :repo "emacs-lsp/lsp-mode"
             :branch "master"
             :files (:defaults "clients/*.el"))
  :commands (lsp lsp-deferred lsp-register-client)
  :init (setq lsp-keymap-prefix "C-c l")
  :bind
  ("<f2>" . lsp-rename)
  :config
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :defer t
  :hook
  (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-doc-use-webkit t)
  :custom-face
  (lsp-ui-doc-background ((t :background "#2e3440")))
  (lsp-ui-peek-header ((t :foreground "#d8dee9" :background "#2e3440")))
  (lsp-ui-peek-highlight ((t :foreground "#2e3440" :background "#88c0d0")))
  (lsp-ui-peek-selection ((t :foreground "#d8dee9" :background "#4c566a"))))

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
