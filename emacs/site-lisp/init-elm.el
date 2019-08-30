;; Elm

(use-package elm-mode
  :after company
  :custom
  (elm-indent-offset 2)
  :config
  (push 'company-elm company-backends))

(provide 'init-elm)
