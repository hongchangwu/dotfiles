(use-package slime-company)

(use-package slime
  :after slime-company
  :custom
  (inferior-lisp-program "sbcl")
  :custom
  (require 'slime-autoloads)
  ;; Also setup the slime-fancy contrib
  (slime-setup '(slime-fancy slime-company))
  ;; (add-to-list 'slime-contribs 'slime-fancy)
  :bind
  (:map slime-mode-map
        ("M-h" . slime-documentation-lookup)))

(provide 'init-lisp)
