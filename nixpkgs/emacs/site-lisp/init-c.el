;;; init-c.el --- init file for C/C++

;;; Commentary:

;; I don't do a lot of C/C++ development, so the setup here is very minimum.

;;; Code:

(defun my-c-mode-common-hook ()
  (setq c-default-style "linux")
  (setq c-basic-offset 4))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(provide 'init-c)

;;; init-c.el ends here
