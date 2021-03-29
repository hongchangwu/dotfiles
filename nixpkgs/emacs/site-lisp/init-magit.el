;;; init-magit.el --- init file for Magit

;;; Commentary:

;; The best Git client, period.

;;; Code:

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-branch-prefer-remote-upstream '("master"))
  (magit-branch-adjust-remote-upstream-alist '(("origin/master" "upstream/master" "master"))))

(provide 'init-magit)

;;; init-magit.el ends here
