;;; init-appearance.el --- init file for customizing the apperance of Emacs

;;; Commentary:

;; Tango color theme is hands down the best color theme for Emacs.
;; Also use `fill-column-indicator` to visually indicate the location of the
;; fill column, although I should switch to the native
;; `display-fill-column-indicator-mode` at some point.

;;; Code:

;; Disable the splash screen
(setq inhibit-splash-screen t)

;; Line numbers and column numbers
(column-number-mode t)
(line-number-mode t)
(global-linum-mode t)

;; Frame size and font
(setq initial-frame-alist
      '((fullscreen . maximized)
        (font . "Inconsolata for Powerline 14")))
(setq default-frame-alist initial-frame-alist)

;; Mode line colors
(set-face-foreground 'mode-line "white")
(set-face-background 'mode-line "#6a5acd")

;; Insert spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Set line width to 80 columns...
(setq fill-column 80)
(setq auto-fill-mode t)

;; Visually show the fill column
(use-package fill-column-indicator
  :hook
  (prog-mode . fci-mode)
  :custom
  (fci-rule-column 90))

;; Abbrev mode
(use-package abbrev
  :straight nil
  :delight abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;; Auto-revert mode
(use-package autorevert
  :straight nil
  :delight auto-revert-mode)

;; Visualize white spaces
(use-package whitespace
  :delight whitespace-mode
  :hook
  (prog-mode . whitespace-mode)
  (before-save . delete-trailing-whitespace)
  :custom
  (whitespace-line-column 80) ;; limit line length
  (whitespace-style '(face lines-tail))
  (show-trailing-whitespace t))

;; Color theme
(use-package tangotango-theme
  :defer t
  :init
  (load-theme 'tangotango t))

;; Colorize compilation buffers
(require 'ansi-color)
(defun colorize-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'compilation-filter-hook 'colorize-buffer)

(provide 'init-appearance)

;;; init-appearance.el ends here
