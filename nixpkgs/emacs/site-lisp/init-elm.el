;;; init-elm.el --- init file for Elm

;;; Commentary:

;; Install `elm-mode`.

;;; Code:

(use-package elm-mode
  :after company
  :custom
  (elm-indent-offset 2))

(provide 'init-elm)

;;; init-elm.el ends here
