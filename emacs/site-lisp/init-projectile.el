;; Projectile

(use-package helm-projectile)

(use-package projectile
  :after helm-projectile
  :delight '(:eval (concat " [" (projectile-project-name) "]"))
  :init
  (projectile-mode +1)
  (helm-projectile-on)
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map)
        ("C-c p" . projectile-command-map))
  :custom
  (projectile-completion-system 'helm)
  (projectile-use-git-grep t))

(provide 'init-projectile)
