;;; init-rust.el --- init file for Shell scripting

;;; Commentary:

;; Enable language server for Bash.

;;; Code:

(use-package sh-script
  :ensure-system-package
  (bash-language-server . "npm i -g bash-language-server")
  :hook
  (sh-mode . lsp)
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2))

(provide 'init-sh)

;;; init-sh.el ends here
