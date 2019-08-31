;; YAML

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :hook
  (yaml-mode
   .
   (lambda ()
     (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(provide 'init-yaml)
