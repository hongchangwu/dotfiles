;; GNU Global interface
(require 'helm-gtags)
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
  (define-key helm-gtags-mode-map (kbd "C-c g >") 'helm-gtags-next-history))

(provide 'init-gtags)
