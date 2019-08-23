;; OCaml

;; Add opam emacs direcotry to load path
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
   (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))))

;; Use the opam installed utop
(setq utop-command "opam config exec -- utop -emacs")
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)
(add-hook 'tuareg-mode-hook
          (lambda()
            ;; Enable the representation of some keywords using fonts
            (when (functionp 'prettify-symbols-mode)
              (prettify-symbols-mode))))
(setq tuareg-prettify-symbols-full t)
(setq tuareg-match-clause-indent 0)
(add-hook 'tuareg-mode-hook 'paredit-nonlisp)
(with-eval-after-load 'tuareg
  (define-key tuareg-mode-map "{" 'paredit-open-curly)
  (define-key tuareg-mode-map "}" 'paredit-close-curly-and-newline)
  (define-key tuareg-mode-map (kbd "M-n") 'tuareg-next-phrase)
  (define-key tuareg-mode-map (kbd "M-p") 'tuareg-previous-phrase))

;; ocp-indent
(require 'ocp-indent)

;; Compilation mode
(require 'compile)
(setq compilation-scroll-output 'first-error)
(setq compilation-always-kill t)
(setq next-error-highlight t)
(add-hook
 'tuareg-mode-hook
 (lambda ()
   (set (make-local-variable 'compile-command)
        (format "cd %s && make" (projectile-project-root)))))

;; Start merlin on ocaml files
(require 'merlin)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)

;; Merlin backend for eldoc
(require 'merlin-eldoc)
(add-hook 'tuareg-mode-hook 'merlin-eldoc-setup)
;; (add-hook 'reason-mode-hook 'merlin-eldoc-setup)

;; dune
(require 'dune)

;; ocamlformat
(require 'ocamlformat)
(add-hook 'tuareg-mode-hook
  (lambda ()
    (define-key tuareg-mode-map (kbd "C-M-<tab>") #'ocamlformat)
    (add-hook 'before-save-hook #'ocamlformat-before-save)))

(provide 'init-ocaml)
