;;; init-flyspell.el --- init file for Flyspell

;;; Commentary:

;; Use Aspell as the backend for Flyspell.

;;; Code:

(use-package flyspell
  :custom
  (ispell-program-name "aspell")
  :config
  (define-key flyspell-mode-map [down-mouse-3] 'flyspell-correct-word)
  (add-hook 'org-mode-hook 'flyspell-mode)
  ;; Enable Flyspell mode for TeX modes such as AUCTeX. Highlights all misspelled words.
  (add-hook 'TeX-mode-hook 'flyspell-mode)
  ;; Enable Flyspell program mode for emacs lisp mode, which highlights all misspelled words in comments and strings.
  (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode))

(use-package flyspell-correct
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-helm
  :after flyspell-correct)

(provide 'init-flyspell)

;;; init-flyspell.el ends here
