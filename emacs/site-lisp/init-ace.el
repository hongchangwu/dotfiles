;;; init-ace.el --- init file ace-window and ace-jump-mode

;;; Commentary:

;; Use `M-o` to jump to the other window and `C-c SPC` to quickly move your
;; cursor to any position based on the first character of a word.

;;; Code:

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(provide 'init-ace)

;;; init-ace.el ends here
