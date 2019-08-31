;; Company mode

;; Fix compatibility issue with fill-column-indicator
(defun on-off-fci-before-company(command)
  (when (string= "show" command)
    (turn-off-fci-mode))
  (when (string= "hide" command)
    (turn-on-fci-mode)))

(use-package company
  :delight company-mode
  :hook
  (after-init . global-company-mode)
  :config
  (advice-add 'company-call-frontends :before #'on-off-fci-before-company))

(provide 'init-company)
