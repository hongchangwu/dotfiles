(use-package sly
  :custom
  (inferior-lisp-program "sbcl")
  :custom
  (require 'sly-autoloads)
  :bind
  (:map sly-mode-map
        ("M-h" . sly-documentation-lookup)))

(provide 'init-lisp)
