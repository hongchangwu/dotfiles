;;; init-rust.el --- init file for Scala

;;; Commentary:

;; Scala and SBT support.  Note: the setup here is out of date.
;; Should use Metals for LSP support.

;;; Code:

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :hook
  (scala-mode . lsp)
  (scala-mode . flycheck-mode))

(use-package sbt-mode
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(provide 'init-scala)

;;; init-scala.el ends here
