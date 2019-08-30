;; JavaScript

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

(use-package js
  :after (flycheck nodejs-repl)
  :hook
  (js-mode . paredit-nonlisp)
  (js-mode . (lambda () (flycheck-mode t)))
  (flycheck-mode . my/use-eslint-from-node-modules)
  :bind
  (:map js-mode-map
        ("{" . paredit-open-curly)
        ("}" . paredit-close-curly-and-newline)
        ("\'" . paredit-singlequote)
        ("C-x C-e" . nodejs-repl-send-last-expression)
        ("C-c C-j" . nodejs-repl-send-line)
        ("C-c C-r" . nodejs-repl-send-region)
        ("C-c C-l" . nodejs-repl-load-file)
        ("C-c C-z" . nodejs-repl-switch-to-repl))
  :custom
  (css-indent-offset 2)
  ;; disable jshint since we prefer eslint checking
  (flycheck-disabled-checkers
   (append flycheck-disabled-checkers
           '(javascript-jshint)))
  ;; disable json-jsonlist checking for json files
  (flycheck-disabled-checkers
   (append flycheck-disabled-checkers
           '(json-jsonlist)))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  ;; use web-mode for plain HTML
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; use web-mode for JSX
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  :custom
  ;; adjust indents for web-mode to 2 spaces
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  :config
  ;; for better jsx syntax-highlighting in web-mode
  ;; - courtesy of Patrick @halbtuerke
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it)))

(use-package web-beautify
  :config
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

(provide 'init-javascript)
