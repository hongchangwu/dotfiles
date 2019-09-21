;; Treemacs

(use-package treemacs
  :defer t
  :bind
  (("M-0" . treemacs-select-window)
   ("C-x 1" . treemacs-delete-other-windows)
   ("<f5>" . treemacs)
   ("C-x t b" . treemacs-bookmark)
   ("C-x t f" . treemacs-find-file)
   ("C-x t t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

(provide 'init-treemacs)
