;; https://github.com/raxod502/straight.el/blob/develop/README.md#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(use-package use-package-ensure-system-package)
(use-package delight)

;; Configure load path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Custom configurations
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
(require 'init-c)
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
(require 'init-rust)
(require 'init-undo-tree)
(require 'init-yaml)
(require 'init-format-all)
(require 'init-eshell)
