;; Projectile
(require 'projectile)
(projectile-mode)
(setq projectile-completion-system 'helm)
(setq projectile-use-git-grep t)
(helm-projectile-on)

(provide 'init-projectile)
