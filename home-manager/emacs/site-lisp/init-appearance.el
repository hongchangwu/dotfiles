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

;; Disable toolbar
(tool-bar-mode -1)

;; Frame size and font
(let ((font-size (if (string= system-type "darwin") "18" "14")))
  (setq initial-frame-alist
        `((fullscreen . maximized)
          (font . ,(format "%s-%s" "Inconsolata for Powerline" font-size))))
  (setq default-frame-alist initial-frame-alist))

;; Color theme
;; Need to make sure the theme is loaded after the frame is created when
;; Emacs is running in daemon mode.
;; https://stackoverflow.com/a/23668935
(use-package nord-theme
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (load-theme 'nord t))))
      (load-theme 'nord t)))

;; Doom mode-line
(use-package doom-modeline
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'doom-modeline-mode)
      (doom-modeline-mode 1))
  (setq doom-modeline-icon t)
  (setq doom-modeline-project-detection 'project))

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
  :bind
  ("C-c C-w" . delete-trailing-whitespace)
  :hook
  (prog-mode . whitespace-mode)
  (before-save . delete-trailing-whitespace)
  :custom
  (whitespace-line-column 80) ;; limit line length
  (whitespace-style '(face lines-tail))
  (show-trailing-whitespace t))

;; Colorize compilation buffers
(require 'ansi-color)
(defun colorize-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'compilation-filter-hook 'colorize-buffer)

;; Tabs
(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-style "bar"
	centaur-tabs-height 32
	centaur-tabs-set-icons t
	centaur-tabs-set-modified-marker t)
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-c t s" . centaur-tabs-switch-group)
  ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups))

(provide 'init-appearance)

;;; init-appearance.el ends here
