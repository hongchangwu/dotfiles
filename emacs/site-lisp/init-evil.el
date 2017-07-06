;; Vim like word search
(require 'evil)
(global-set-key (kbd "C-*") 'evil-search-word-forward)
(global-set-key (kbd "C-#") 'evil-search-word-backward)

(provide 'init-evil)
