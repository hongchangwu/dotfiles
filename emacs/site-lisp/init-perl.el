;; Use cperl mode instead of the default perl mode
(defalias 'perl-mode 'cperl-mode)

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

(provide 'init-perl)
