;; OCaml
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

;; Start merlin on ocaml files
(require 'merlin)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)

(provide 'init-ocaml)
