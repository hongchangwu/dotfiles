;;; init-appearance.el --- init file for customizing the apperance of Emacs

;;; Commentary:

;; Use `fill-column-indicator` to visually indicate the location of the
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
(let ((font-size (if (string= system-type "darwin") "18" "14")))
  (setq initial-frame-alist
        `((fullscreen . maximized)
          (font . ,(format "%s-%s" "Inconsolata for Powerline" font-size))))
  (setq default-frame-alist initial-frame-alist))

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-project-detection 'project))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-vibrant t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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
(use-package nord-theme
  :defer t
  :init
  (load-theme 'nord t))

;; Colorize compilation buffers
(require 'ansi-color)
(defun colorize-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'compilation-filter-hook 'colorize-buffer)

(provide 'init-appearance)

;;; init-appearance.el ends here
