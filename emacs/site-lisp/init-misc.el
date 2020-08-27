;;; init-misc.el -- init file for miscellaneous configurations

;;; Commentary:

;; Mostly convenience bindings for text editing.
;; Make % match brackets like Vim.

;;; Code:

;; For easier regex search/replace
(defalias 'qrr 'query-replace-regexp)

;; Switch between two most recent buffers
(global-set-key (kbd "M-`") 'mode-line-other-buffer)

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

;; turn autoindenting on
(global-set-key "\r" 'newline-and-indent)

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

(provide 'init-misc)

;;; init-misc.el ends here
