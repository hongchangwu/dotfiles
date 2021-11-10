;;; init-ocaml.el --- init file for OCaml

;;; Commentary:

;; The native OCaml LSP server is relatively mature by now so I'm choosing
;; lsp-mode over merlin and merlin-eldoc.

;;; Code:

(use-package tuareg
  :ensure-system-package
  (ocamllsp . "opam pin add ocaml-lsp-server https://github.com/ocaml/ocaml-lsp.git && opam install ocaml-lsp-server")
  :init
  ;; Add opam emacs directory to load path
  (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))))
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  :delight utop-minor-mode
  :hook
  (tuareg-mode . lsp)
  (tuareg-mode . lsp-lens-mode)
  (tuareg-mode . utop-minor-mode)
  (tuareg-mode
   .
   (lambda()
     ;; Enable the representation of some keywords using fonts
     (when (functionp 'prettify-symbols-mode)
       (prettify-symbols-mode))))
  (tuareg-mode . tree-sitter-mode)
  (tuareg-mode . tree-sitter-hl-mode)
  (tuareg-mode . paredit-nonlisp)
  :bind
  (:map tuareg-mode-map
        ("{" . paredit-open-curly)
        ("}" . paredit-close-curly-and-newline)
        ("M-n" . tuareg-next-phrase)
        ("M-p" . tuareg-previous-phrase))
  :custom
  ;; Use the opam installed utop
  (utop-command "opam config exec -- utop -emacs")
  (tuareg-prettify-symbols-full t)
  (tuareg-match-clause-indent 0))

;; ocp-indent
(use-package ocp-indent
  :after tuareg)

;; Dune
(use-package dune
  :straight nil)

;; ocamlformat
(use-package ocamlformat
  :straight nil
  :after tuareg
  :bind
  (:map tuareg-mode-map
        ("C-c C-f" . ocamlformat))
  ;; :hook
  ;; (before-save . ocamlformat-before-save)
  )

(provide 'init-ocaml)

;;; init-ocaml.el ends here
