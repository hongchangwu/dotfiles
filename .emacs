;; Disable the splash screen
(setq inhibit-splash-screen t)

;; Line numbers and column numbers
(column-number-mode t)
(line-number-mode t)
(global-linum-mode t)

;; Frame size and font
(setq initial-frame-alist
      '((left . 100)
        (top . 50)
        (height . 40)
        (width . 120)
        (font . "Inconsolata for Powerline 16")))
(setq default-frame-alist initial-frame-alist)

;; Colors
(custom-set-faces
 '(mode-line ((t (:foreground "white" :background "#6a5acd" :box nil)))))

;; Insert spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Set line width to 78 columns...
(setq fill-column 78)
(setq auto-fill-mode t)

;; Trailing whitespaces
(setq-default show-trailing-whitespace t)
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Package
(when (>= emacs-major-version 24)
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
        '(clojure-mode
          clojure-mode-extra-font-locking
          company
          company-ghc
          elm-mode
          ensime
          ess
          evil
          ghc
          haskell-mode
          helm
          helm-ag
          helm-gtags
          helm-projectile
          hindent
          json-mode
          magit
          merlin
          paredit
          powerline
          projectile
          react-snippets
          robe
          sr-speedbar
          tangotango-theme
          tuareg
          utop
          web-beautify
          web-mode
          yaml-mode))
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

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

(global-set-key (kbd "C-c w k") 'windmove-up)
(global-set-key (kbd "C-c w j") 'windmove-down)
(global-set-key (kbd "C-c w l") 'windmove-right)
(global-set-key (kbd "C-c w h") 'windmove-left)

;; Highlight matching brackets
(show-paren-mode 1)
(setq show-paren-delay 0)

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
(when (require 'evil nil t)
  (global-set-key (kbd "C-*") 'evil-search-word-forward)
  (global-set-key (kbd "C-#") 'evil-search-word-backward))

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
(when (require 'haskell-mode nil t)
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (eval-after-load 'haskell-mode
    '(progn
       (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
       (add-hook 'before-save-hook 'haskell-mode-stylish-buffer)))
  (custom-set-variables
   '(haskell-interactive-popup-errors nil)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-process-type 'auto)
   '(haskell-tags-on-save t)
   '(haskell-font-lock-symbols t)))

(when (require 'hindent nil t)
  (add-hook 'haskell-mode-hook #'hindent-mode))

(when (require 'company-ghc nil t)
  (add-to-list 'company-backends 'company-ghc)
  (custom-set-variables '(company-ghc-show-info t)))

(require 'hs-lint nil t)

;; (require 'shm)
;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
;; (add-hook 'haskell-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "RET") 'shm/newline-indent-proxy)))

;; Clojure mode
(when (require 'clojure-mode nil t)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'nrepl-mode-hook 'paredit-mode))
(when (require 'clojure-mode-extra-font-locking nil t)
  (setq nrepl-log-messages t)
  (setq nrepl-hide-special-buffers t)
  (setq cider-show-error-buffer 'except-in-repl))

;; Ruby mode
(when (require 'robe nil t)
  (push 'company-robe company-backends)
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-mode-hook 'eldoc-mode)
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (add-hook 'enh-ruby-mode-hook 'eldoc-mode))

;; Octave
(setq auto-mode-alist
      (cons
       '("\\.m$" . octave-mode)
       auto-mode-alist))

;; Powerline
(when (require 'powerline nil t)
  (powerline-center-evil-theme))

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
(setq tuareg-match-clause-indent 0)
(with-eval-after-load 'tuareg
  (define-key tuareg-mode-map (kbd "M-n") 'tuareg-next-phrase)
  (define-key tuareg-mode-map (kbd "M-p") 'tuareg-previous-phrase))

;; Load merlin-mode
(when (require 'merlin nil t)
  ;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t))

;; Magit
(when (require 'magit nil t)
  (global-set-key (kbd "C-x g") 'magit-status))

;; Helm
(when (and (require 'helm nil t)
           (require 'helm-config nil t))
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-c h o") 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (global-set-key (kbd "M-x") 'helm-M-x)
  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
  (setq helm-M-x-fuzzy-match t)
  (helm-mode 1))

;; Helm Ag
(when (require 'helm-ag nil t)
  (setq helm-ag-insert-at-point (quote symbol)))

;; Projectile
(when (and (require 'projectile nil t)
           (require 'helm-projectile nil t))
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (setq projectile-use-git-grep t)
  (helm-projectile-on))

;; GNU Global interface
(when (require 'helm-gtags nil t)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'java-mode-hook 'helm-gtags-mode)
  (add-hook 'js-mode-hook 'helm-gtags-mode)
  (add-hook 'python-mode-hook 'helm-gtags-mode)
  (add-hook 'tuareg-mode-hook 'helm-gtags-mode)
  (custom-set-variables
   '(helm-gtags-prefix-key "\C-cg")
   '(helm-gtags-suggested-key-mapping t)
   '(helm-gtags-auto-update t)
   '(helm-gtags-use-input-at-cursor t)
   '(helm-gtags-pulse-at-cursor t))
  (with-eval-after-load 'helm-gtags
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
    (define-key helm-gtags-mode-map (kbd "C-c g c") 'helm-gtags-create-tags)
    (define-key helm-gtags-mode-map (kbd "C-c g <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c g >") 'helm-gtags-next-history)))

;; Semantic
(add-hook 'c-mode-hook 'semantic-mode)
(add-hook 'c++-mode-hook 'semantic-mode)
(add-hook 'java-mode-hook 'semantic-mode)

;; Elm
(when (require 'elm nil t)
  (custom-set-variables '(elm-indent-offset 2))
  (add-to-list 'company-backends 'company-elm))

;; JavaScript
(when (require 'flycheck nil t)
  (add-hook 'js-mode-hook
            (lambda () (flycheck-mode t)))
  ;; turn on flychecking globally
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist)))
  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))
(when (require 'web-mode nil t)
  ;; use web-mode for .jsx files
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  ;; adjust indents for web-mode to 2 spaces
  (defun my-web-mode-hook ()
    "Hooks for Web mode. Adjust indents"
     ;;; http://web-mode.org/
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  ;; for better jsx syntax-highlighting in web-mode
  ;; - courtesy of Patrick @halbtuerke
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it)))
(when (require 'web-beautify nil t)
  (eval-after-load 'js2-mode
    '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
  ;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
  (eval-after-load 'js
    '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'json-mode
    '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'sgml-mode
    '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
  (eval-after-load 'web-mode
    '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
  (eval-after-load 'css-mode
    '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css)))
