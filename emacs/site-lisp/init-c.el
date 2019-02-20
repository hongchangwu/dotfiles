;; C/C++

(defun my-c-mode-common-hook ()
 (c-set-offset 'substatement-open 0)

 (setq c-basic-offset 4)
 (setq c-indent-level 4)
 )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(provide 'init-c)
