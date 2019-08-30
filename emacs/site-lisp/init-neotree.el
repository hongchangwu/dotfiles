;; Neotree

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(use-package neotree
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  (projectile-switch-project-action 'neotree-projectile-action)
  :bind
  ([f8] . neotree-project-dir))

(provide 'init-neotree)
