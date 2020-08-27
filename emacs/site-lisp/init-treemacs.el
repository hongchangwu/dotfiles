;;; init-treemacs.el --- Treemacs

;;; Commentary:

;; Treemacs is a file and project explorer similar to NeoTree or vim’s NerdTree.

;;; Code:

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
        (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind
  (("M-0"     . treemacs-select-window)
   ("C-x t 1" . treemacs-delete-other-windows)
   ("<f8>"    . treemacs)
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

;;; init-treemacs.el ends here
