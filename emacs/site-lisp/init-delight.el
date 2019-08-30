(use-package delight
  :config
  (delight
   '((auto-revert-mode nil)
     (company-mode nil company)
     (helm-mode nil helm)
     (helm-gtags-mode nil helm-gtags)
     (projectile-mode nil projectile)
     (undo-tree-mode nil undo-tree)
     (whitespace-mode nil whitespace)
     (yas-minor-mode nil yasnippet)
     (emacs-lisp-mode "Elisp" :major))))

(provide 'init-delight)
