;;; init-perl.el --- init file for Perl

;;; Commentary:

;; The settings here are copied from the book "Perl Best Practices".

;;; Code:

(use-package cperl-mode
  :init
  ;; Use cperl mode instead of the default perl mode
  (defalias 'perl-mode 'cperl-mode)
  :custom
  ;; Use 4 space indents via cperl mode
  (cperl-close-paren-offset 4)
  (cperl-continued-statement-offset 4)
  (cperl-indent-level 4)
  (cperl-indent-parens-as-block t)
  (cperl-tab-always-indent t))

(provide 'init-perl)

;;; init-perl.el ends here
