;; Company mode

;; Fix compatibility issue with fill-column-indicator
(defun on-off-fci-before-company(command)
  (when (string= "show" command)
    (turn-off-fci-mode))
  (when (string= "hide" command)
    (turn-on-fci-mode)))

(use-package company
  :delight company-mode
  :hook
  (prog-mode . company-mode)
  :custom
  (company-tooltip-align-annotations t)
  :config
  (advice-add 'company-call-frontends :before #'on-off-fci-before-company)
  ;; https://github.com/Sarcasm/.emacs.d/blob/544591c02faa019872a48ffecfe559014a43380a/theme/sarcasm-theme.el
  (custom-set-faces
   `(company-echo-common ((t (:underline t))))
   `(company-preview ((t (:inherit shadow))))
   `(company-preview-common ((t (:inherit company-preview :underline t))))
   `(company-scrollbar-bg ((t (:inherit company-tooltip :background "SteelBlue3"))))
   `(company-scrollbar-fg ((t (:background "DeepSkyBlue4"))))
   `(company-template-field ((t (:background "DeepSkyBlue3" :foreground "black"))))
   `(company-tooltip ((t (:background "LightSteelBlue1" :foreground "dark slate gray"))))
   `(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "slate gray"))))
   `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :background "LightSteelBlue3"))))
   `(company-tooltip-common ((t (:inherit company-tooltip :underline t))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :underline t))))
   `(company-tooltip-mouse ((t (:inherit company-tooltip-selection))))
   `(company-tooltip-selection ((t (:inherit company-tooltip :background "LightSteelBlue3"))))))

(provide 'init-company)
