;; Haskell mode

(use-package haskell-mode
  :ensure-system-package
  ((stack . "curl -sSL https://get.haskellstack.org/ | sh")
   (hasktags . "stack install hasktags"))
  :init
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . haskell-doc-mode)
  (haskell-mode . paredit-nonlisp)
  :bind
  (:map haskell-mode-map
        ("{" . paredit-open-curly)
        ("}" . paredit-close-curly-and-newline)
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
  :ensure-system-package (hindent . "stack install hindent")
  :after haskell-mode
  :hook
  (haskell-mode . hindent-mode))

;; Intero
(use-package intero
  :after haskell-mode
  :hook
  (haskell-mode . intero-mode)
  :custom
  (intero-extra-ghc-options "-i."))

(provide 'init-haskell)
