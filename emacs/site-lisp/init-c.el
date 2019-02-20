;; C/C++

(defun my-c-mode-common-hook ()
 (setq c-default-style "linux"
 (setq c-basic-offset 4)
 )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(provide 'init-c)
