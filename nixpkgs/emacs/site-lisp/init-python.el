;;; init-python.el --- init file for Python

;;; Commentary:

;; Enable `pyls` for language server support.

;;; Code:

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(use-package pyenv-mode
  :ensure-system-package
  (pyenv . "curl https://pyenv.run | bash")
  :init
  (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set))

(use-package python-mode
  :hook
  (python-mode . (lambda () (flymake-mode -1)))
  (python-mode . flycheck-mode)
  (python-mode . lsp)
  (python-mode . pyenv-mode))

(provide 'init-python)

;;; init-python.el ends here
