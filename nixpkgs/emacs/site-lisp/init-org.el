;;; init-org.el --- init file for Org mode

;;; Commentary:

;; Use org-bullets-mode for nice UTF-8 bullets.

;;; Code:

(use-package org-bullets
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(provide 'init-org)

;;; init-org.el ends here
