;; Haskell mode

(use-package haskell-mode
  :init
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  :hook
  (haskell-mode . (lambda () (ghc-init)))
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . turn-on-haskell-doc-mode)
  (haskell-mode . paredit-nonlisp)
  (before-save . haskell-mode-stylish-buffer)
  :bind
  (:map haskell-mode-map
        ("{" . paredit-open-curly)
        ("}" . paredit-close-curly-and-newline)
        ("\'" . paredit-singlequote)
        ([f8] . haskell-navigate-imports)
        ("C-c C-h" . helm-hoogle))
  :custom
  (haskell-interactive-popup-errors nil)
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-process-type 'auto)
  (haskell-tags-on-save t)
  (haskell-font-lock-symbols t))

(use-package hindent
  :requires haskell-mode
  :hook
  (haskell-mode . hindent-mode))

(use-package company-ghc
  :after (company haskell-mode)
  :custom
  (company-ghc-show-info t)
  :config
  (push 'company-ghc company-backends))

(provide 'init-haskell)
