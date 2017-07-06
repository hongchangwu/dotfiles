;; Disable the splash screen
(setq inhibit-splash-screen t)

;; Line numbers and column numbers
(column-number-mode t)
(line-number-mode t)
(global-linum-mode t)

;; Frame size and font
(setq initial-frame-alist
      '((left . 100)
        (top . 50)
        (height . 40)
        (width . 120)
        (font . "Inconsolata for Powerline 16")))
(setq default-frame-alist initial-frame-alist)

;; Colors
(custom-set-faces
 '(mode-line ((t (:foreground "white" :background "#6a5acd" :box nil)))))

;; Insert spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Set line width to 78 columns...
(setq fill-column 78)
(setq auto-fill-mode t)

;; Trailing whitespaces
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Color theme
(load-theme 'tangotango t)

(provide 'init-appearance)
