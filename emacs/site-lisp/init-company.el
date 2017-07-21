;; Company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Fix compatibility issue with fill-column-indicator
(defun on-off-fci-before-company(command)
  (when (string= "show" command)
    (turn-off-fci-mode))
  (when (string= "hide" command)
    (turn-on-fci-mode)))

(advice-add 'company-call-frontends :before #'on-off-fci-before-company)

(provide 'init-company)
