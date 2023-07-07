;;; init-rust.el --- init file for Semantic mode

;;; Commentary:

;; Semantic allows Emacs to parse the source code issue language-aware
;; editing commands.

;;; Code:

(use-package semantic
  :hook
  ((c-mode c++-mode java-mode) . semantic-mode))

(provide 'init-semantic)

;;; init-semantic.el ends here
