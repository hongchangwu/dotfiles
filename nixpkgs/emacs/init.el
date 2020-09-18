;;; init.el --- main init file

;;; Commentary:

;; I'm using straight.el together with use-package.
;; This file simply bootstraps straight.el and adds a few common packages.
;; The custom init files are located under ~/.emacs.d/site-lisp

;;; Code:

;; https://github.com/raxod502/straight.el#getting-started
(setq straight-cache-autoloads t
      straight-repository-branch "develop"
      straight-use-package-by-default t)
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

;; https://github.com/raxod502/straight.el#integration-with-use-package
(straight-use-package 'use-package)
(use-package use-package-ensure-system-package)
(use-package delight)

;; Configure load path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Custom init files

(require 'init-ace)
(require 'init-appearance)
(require 'init-avy)
(require 'init-c)
(require 'init-clojure)
(require 'init-company)
(require 'init-eldoc)
(require 'init-elixir)
(require 'init-elm)
(require 'init-eshell)
(require 'init-flycheck)
(require 'init-flyspell)
(require 'init-go)
(require 'init-gtags)
(require 'init-haskell)
(require 'init-helm)
(require 'init-javascript)
(require 'init-lisp)
(require 'init-lsp)
(require 'init-magit)
(require 'init-misc)
(require 'init-nix)
(require 'init-ocaml)
(require 'init-octave)
(require 'init-org)
(require 'init-paredit)
(require 'init-perl)
(require 'init-projectile)
(require 'init-python)
(require 'init-ruby)
(require 'init-rust)
(require 'init-scala)
(require 'init-semantic)
(require 'init-sh)
(require 'init-treemacs)
(require 'init-undo-tree)
(require 'init-yaml)
(require 'init-yasnippet)

;;; init.el ends here
