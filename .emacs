;; Disable the splash screen
(setq inhibit-splash-screen t)

;; Line numbers and column numbers
(column-number-mode t)
(line-number-mode t)
(global-linum-mode t)

;; Frame size
(setq initial-frame-alist '((left . 100)
                            (top . 50)
                            (height . 40)
                            (width . 120)))

;; Font
(set-default-font "Inconsolata-g for Powerline 14")

;; Insert spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Set line width to 78 columns...
(setq fill-column 78)
(setq auto-fill-mode t)

;; Trailing whitespaces
(setq-default show-trailing-whitespace t)
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Package
(require 'package)
;; list of package repositories
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
;; activate all packages
(package-initialize)
;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;; install all required packages
(setq package-list
      '(ack
        clojure-mode
        clojure-mode-extra-font-locking
        company
        company-ghc
        ess
        evil
        ghc
        haskell-mode
        hindent
        merlin
        paredit
        powerline
        robe
        tangotango-theme
        tuareg
        utop))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Load path
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Color theme
(load-theme 'tangotango t)

;; Sh Mode
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2
                  sh-indentation 2)))

;; CTags
(require 'subr-x)
(setq path-to-ctags (string-trim-right (shell-command-to-string "which ctags")))
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name))))

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)

;; ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'racket-mode-hook           #'enable-paredit-mode)

;; For easier regex search/replace
(defalias 'qrr 'query-replace-regexp)

;; Switch between two most recent buffers
(global-set-key (kbd "M-o")  'mode-line-other-buffer)

(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c h") 'windmove-left)

;; insert before current line and indent
(defun insert-before-and-indent ()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "C-x C-o") 'insert-before-and-indent)

;; Backward kill line
(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key (kbd "M-k") 'backward-kill-line)

;; Use cperl mode instead of the default perl mode
(defalias 'perl-mode 'cperl-mode)

;; turn autoindenting on
(global-set-key "\r" 'newline-and-indent)

;; Use 4 space indents via cperl mode
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-close-paren-offset 4)
 '(cperl-continued-statement-offset 4)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-tab-always-indent t)
 '(custom-safe-themes (quote ("52d707d93c3cd09ce0485a70b7bf52fbd7966d46144e05c2c3bcc5b70b07825f" default))))

;; Vim like word search
(require 'evil)
(global-set-key (kbd "C-*") 'evil-search-word-forward)
(global-set-key (kbd "C-#") 'evil-search-word-backward)

;; Use % to match various kinds of brackets...
;; See: http://www.lifl.fr/~hodique/uploads/Perso/patches.el
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (self-insert-command (or arg 1))))))

;; Haskell mode
(require 'haskell-mode)
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))
(custom-set-variables
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-type 'cabal-repl)
 '(haskell-tags-on-save t))

(require 'hindent)
(add-hook 'haskell-mode-hook #'hindent-mode)

(require 'company-ghc)
(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))

;; (require 'shm)
;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
;; (add-hook 'haskell-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "RET") 'shm/newline-indent-proxy)))

;; Clojure mode
(require 'clojure-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(require 'clojure-mode-extra-font-locking)
(setq nrepl-log-messages t)
(setq nrepl-hide-special-buffers t)
(setq cider-show-error-buffer 'except-in-repl)

;; Ruby mode
(push 'company-robe company-backends)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'eldoc-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'eldoc-mode)

;; ESS
(require 'ess-site)

;; Octave
(setq auto-mode-alist
      (cons
       '("\\.m$" . octave-mode)
       auto-mode-alist))

;; Powerline
(require 'powerline)
(powerline-center-evil-theme)

;; OCaml
;; Use the opam installed utop
(setq utop-command "opam config exec -- utop -emacs")
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)
;; Load merlin-mode
(require 'merlin)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
