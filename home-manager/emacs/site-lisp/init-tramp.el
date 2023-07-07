;;; init-tramp.el --- init file for TRAMP

;;; Commentary:

;;; Code:

(use-package tramp
  :custom
  (vs-handled-backends '(Git)))

;; C-x C-f /docker:user@container:/path/to/file
;; where
;;   user           is the user that you want to use (optional)
;;   container      is the id or name of the container
(use-package docker-tramp
  :after tramp)

(provide 'init-tramp)

;;; init-tramp.el ends here
