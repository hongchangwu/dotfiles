;; init-projectile.el --- init file for Projectile

;;; Commentary:

;; Projectile with Helm integration.

;;; Code:

(use-package helm-projectile)

(use-package projectile
  :after helm-projectile
  :delight '(:eval (concat " [" (projectile-project-name) "]"))
  :init
  (projectile-mode +1)
  (helm-projectile-on)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  (("<f5>" . projectile-run-project)
   ("<f6>" . projectile-compile-project)
   ("<f7>" . projectile-test-project))
  (:map projectile-command-map
        ("t" . projectile-test-project))
  :custom
  (projectile-completion-system 'helm)
  (projectile-use-git-grep t))

(provide 'init-projectile)

;;; init-projectile.el ends here
