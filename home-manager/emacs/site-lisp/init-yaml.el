;;; init-yaml.el --- init file for YAML

;;; Commentary:

;; Believe it or not YAML mode is not built-in.

;;; Code:

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :hook
  (yaml-mode
   .
   (lambda ()
     (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(provide 'init-yaml)

;;; init-yaml.el ends here
