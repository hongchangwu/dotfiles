(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Load path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'init-appearance)
(require 'init-misc)
(require 'init-company)
(require 'init-eldoc)
(require 'init-paredit)
(require 'init-yasnippet)
(require 'init-magit)
(require 'init-helm)
(require 'init-projectile)
(require 'init-gtags)
(require 'init-semantic)
(require 'init-neotree)
(require 'init-powerline)
(require 'init-sh)
(require 'init-perl)
(require 'init-evil)
(require 'init-org)
(require 'init-haskell)
(require 'init-clojure)
(require 'init-ruby)
(require 'init-octave)
(require 'init-ocaml)
(require 'init-elm)
(require 'init-elixir)
(require 'init-javascript)
