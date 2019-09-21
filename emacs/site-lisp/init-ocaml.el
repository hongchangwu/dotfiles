;; OCaml

(use-package tuareg
  :ensure-system-package (opam (utop . "opam install utop"))
  :init
  ;; Add opam emacs direcotry to load path
  (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))))
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  :delight utop-minor-mode
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
  (tuareg-prettify-symbols-full t)
  (tuareg-match-clause-indent 0))

;; ocp-indent
(use-package ocp-indent
  :ensure-system-package (ocp-indent . "opam install ocp-indent")
  :after tuareg)

;; Compilation mode
(use-package compile
  :after (tuareg projectile)
  :hook
  (tuareg-mode
   .
   (lambda ()
     (set (make-local-variable 'compile-command)
          (format "cd %s && make" (projectile-project-root)))))
  (compilation-mode . (lambda () (setq show-trailing-whitespace nil)))
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  (next-error-highlight t))

;; Start merlin on ocaml files
(use-package merlin
  :straight nil
  :ensure-system-package (ocamlmerlin . "opam install merlin")
  :after tuareg
  :hook
  ((caml-mode tuareg-mode reason-mode) . merlin-mode))

;; Merlin backend for eldoc
(use-package merlin-eldoc
  :after merlin
  :hook
  (merlin-mode . merlin-eldoc-setup))

;; Dune
(use-package dune
  :straight nil
  :ensure-system-package (dune . "opam install dune"))

;; ocamlformat
(use-package ocamlformat
  :straight nil
  :ensure-system-package (ocamlformat . "opam install ocamlformat")
  :after tuareg
  :bind
  (:map tuareg-mode-map
        ("C-c C-f" . ocamlformat))
  ;; :hook
  ;; (before-save . ocamlformat-before-save)
  )

(use-package reason-mode
  :init
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  :delight npm-mode
  :hook
  (reason-mode . lsp)
  (reason-mode . npm-mode)
  (reason-mode . utop-minor-mode)
  (reason-mode . flycheck-mode)
  (before-save . refmt-before-save)
  :bind
  (:map reason-mode-map
        ("C-c C-f" . refmt))
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "reason-language-server")
    :major-modes '(reason-mode)
    :notification-handlers (ht ("client/registerCapability" 'ignore))
    :priority 1
    :server-id 'reason-ls)))

(provide 'init-ocaml)
