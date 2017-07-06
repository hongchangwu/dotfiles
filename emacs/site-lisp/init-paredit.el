;; ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'racket-mode-hook           #'enable-paredit-mode)
(with-eval-after-load 'paredit
  (define-key paredit-mode-map ";" nil))

(defun paredit-singlequote (&optional n)
  "Insert a pair of single-quotes."
  (interactive "P")
  (cond ((paredit-in-string-p)
         (if (eq (point) (- (paredit-enclosing-string-end) 1))
             (forward-char)             ; Just move past the closing quote.
           ;; Don't split a \x into an escaped backslash and a string end.
           (if (paredit-in-string-escape-p) (forward-char))
           (insert ?\\ ?\' )))
        ((paredit-in-comment-p)
         (insert ?\' ))
        ((not (paredit-in-char-p))
         (paredit-insert-pair n ?\' ?\' 'paredit-forward-for-quote))))

(defun paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(provide 'init-paredit)
