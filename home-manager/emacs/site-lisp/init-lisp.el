;;; init-lisp.el --- init file for Common Lisp

;;; Commentary:

;; Sly is a fork of SLIME with improved REPL.

;;; Code:

(use-package sly
  :custom
  (inferior-lisp-program "sbcl")
  :custom
  (require 'sly-autoloads)
  :bind
  (:map sly-mode-map
        ("M-h" . sly-documentation-lookup)))

(provide 'init-lisp)

;;; init-lisp.el ends here
