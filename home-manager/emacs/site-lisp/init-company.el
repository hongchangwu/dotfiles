;;; init-company.el --- init file for Company

;;; Commentary:

;; Turn on `company-mode` for all `prog-mode` and customize the faces.

;;; Code:

;; Fix compatibility issue with fill-column-indicator
(defun on-off-fci-before-company(command)
  (when (string= "show" command)
    (turn-off-fci-mode))
  (when (string= "hide" command)
    (turn-on-fci-mode)))

(use-package company
  :delight company-mode
  :hook
  (prog-mode . company-mode)
  :bind
  ("C-c y" . company-yasnippet)
  :custom
  (company-tooltip-align-annotations t)
  :config
  (advice-add 'company-call-frontends :before #'on-off-fci-before-company)
  ;; Based on the Nord color palette https://www.nordtheme.com/
  (custom-set-faces
   `(company-echo-common ((t (:underline t))))
   `(company-preview ((t (:inherit shadow))))
   `(company-preview-common ((t (:inherit company-preview :underline t))))
   `(company-scrollbar-bg ((t (:background "#3b4252"))))
   `(company-scrollbar-fg ((t (:background "#4c566a"))))
   `(company-tooltip ((t (:background "#3b4252" :foreground "#d8dee9"))))
   `(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "#81a1c1"))))
   `(company-tooltip-annotation-selection ((t (:foreground "#81a1c1" :background "#4c566a"))))
   `(company-tooltip-common ((t (:inherit company-tooltip :underline t))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :underline t))))
   `(company-tooltip-mouse ((t (:inherit company-tooltip-selection))))
   `(company-tooltip-selection ((t (:inherit company-tooltip :background "#4c566a"))))))

(provide 'init-company)

;;; init-company.el ends here
