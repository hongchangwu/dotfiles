;;; init-haskell.el --- init file for Haskell

;;; Commentary:

;; I'm still conflicted about choosing between cabal and stack.
;; The setup here is a compromise between the two.  I use cabal to install
;; system-wide packages and use stack for project-driven development.

;;; Code:

(use-package haskell-mode
  :ensure-system-package
  (ghcup . "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh")
  :init
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . lsp-deferred)
  (haskell-mode . paredit-nonlisp)
  :bind
  (:map haskell-mode-map
        ("{" . paredit-open-curly)
        ("}" . paredit-close-curly-and-newline)
        ("C-c C-h" . helm-hoogle))
  :custom
  (haskell-interactive-popup-errors nil)
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-process-type 'auto)
  (haskell-tags-on-save t)
  (haskell-font-lock-symbols t))

(use-package lsp-haskell
  :after haskell-mode)

;; HLint
(use-package hs-lint
  :straight (:host github
             :repo "ndmitchell/hlint"
             :branch "master"
             :files ("data/hs-lint.el"))
  :after haskell-mode
  :bind
  (:map haskell-mode-map
        ("C-c l" . hs-lint)))

(provide 'init-haskell)

;;; init-haskell.el ends here
