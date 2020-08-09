;;; init-helm.el --- init file for Helm-related packages

;;; Commentary:

;; Bind `C-c h` to `helm-command-prefix` and rebind `M-x` to `helm-command-prefix-key`.

;;; Code:

(use-package helm
  :delight helm-mode
  :bind
  (("C-c h" . helm-command-prefix)
   ;; ("C-c h o" . helm-occur)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files)
   ("M-x" . 'helm-M-x))
  (:map helm-map
        ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
        ("C-i" . helm-execute-persistent-action) ; make TAB work in terminal
        ("C-z"  . helm-select-action) ; list actions using C-z
        )
  :custom
  (helm-M-x-fuzzy-match t)
  :config
  (require 'helm-config)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point)))

(use-package ag
  :ensure-system-package ag)

(use-package helm-ag
  :after (helm ag)
  :custom
  (helm-ag-insert-at-point (quote symbol)))

(use-package ripgrep
  :ensure-system-package (rg . ripgrep))

(use-package helm-rg
  :after (helm ripgrep))

(provide 'init-helm)

;;; init-helm.el ends here
