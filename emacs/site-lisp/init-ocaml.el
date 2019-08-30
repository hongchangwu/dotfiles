;; OCaml

(use-package tuareg
  :init
  ;; Add opam emacs direcotry to load path
  (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))))
  :hook
  (tuareg-mode . utop-minor-mode)
  (tuareg-mode
   .
   (lambda()
     ;; Enable the representation of some keywords using fonts
     (when (functionp 'prettify-symbols-mode)
       (prettify-symbols-mode))))
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
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)  
  (tuareg-prettify-symbols-full t)
  (tuareg-match-clause-indent 0))

;; ocp-indent
(use-package ocp-indent
  :after tuareg)

;; Compilation mode
(use-package compile
  :after tuareg
  :hook
  (tuareg-mode
   .
   (lambda ()
     (set (make-local-variable 'compile-command)
          (format "cd %s && make" (projectile-project-root)))))
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  (next-error-highlight t))

;; Start merlin on ocaml files
(use-package merlin
  :after tuareg
  :hook
  ((caml-mode tuareg-mode reason-mode) . merlin-mode))

;; Merlin backend for eldoc
(use-package merlin-eldoc
  :after merlin
  :hook
  (merlin-mode . merlin-eldoc-setup))

;; Dune
(use-package dune)

;; ocamlformat
(use-package ocamlformat
  :bind
  (:map tuareg-mode-map
        ("C-M-<tab>" . ocamlformat))
  ;; :hook
  ;; (before-save . ocamlformat-before-save)
  )

(provide 'init-ocaml)
