;; ElDoc

(use-package eldoc
  :hook
  ((emacs-lisp-mode lisp-interaction-mode ielm-mode) . turn-on-eldoc-mode))

(provide 'init-eldoc)
