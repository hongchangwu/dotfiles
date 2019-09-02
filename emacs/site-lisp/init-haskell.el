;; Haskell mode

(use-package haskell-mode
  :ensure-system-package
  ((ghcup . "curl https://get-ghcup.haskell.org -sSf | sh")
   (stack . "curl -sSL https://get.haskellstack.org/ | sh")
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

;; hindent
(use-package hindent
  :ensure-system-package (hindent . "stack install hindent")
  :after haskell-mode
  :hook
  (haskell-mode . hindent-mode)
  :custom
  (hindent-reformat-buffer-on-save t))

;; HLint
(use-package hs-lint
  :straight nil
  :ensure-system-package (hlint . "stack install hlint")
  :after haskell-mode
  :bind
  (:map haskell-mode-map
        ("C-c l" . hs-lint)))

;; Intero
(use-package intero
  :after haskell-mode
  :hook
  (haskell-mode . intero-mode)
  (haskell-mode . intero-mode-blacklist)
  :custom
  (intero-blacklist '("projecteuler")))

(provide 'init-haskell)
