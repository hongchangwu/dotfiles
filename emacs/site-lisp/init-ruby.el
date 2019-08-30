;; Ruby mode

(use-package robe
 :after company
 :hook
 (ruby-mode . robe-mode)
 (ruby-mode . eldoc-mode)
 (enh-ruby-mode . robe-mode)
 (enh-ruby-mode . eldoc-mode)
 :config
 (push 'company-robe company-backends))

(provide 'init-ruby)
