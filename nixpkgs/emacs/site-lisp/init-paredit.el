;;; init-paredit.el --- init file for ParEdit

;;; Commentary:

;; Turn on paredit for Lisp dialects.  Also define `paredit-nonlisp` for use in
;; non-Lisp languages.

;;; Code:

(use-package paredit
  :init
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  :hook
  ((dune-mode
    emacs-lisp-mode
    eval-expression-minibuffer-setup
    ielm-mode
    lisp-interaction-mode
    lisp-mode
    racket-mode
    scheme-mode)
   .
   enable-paredit-mode)
  :bind
  (:map paredit-mode-map
        (";" . nil)
        ("M-?" . nil)))

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

;;; init-paredit.el ends here
