;;; init-magit.el --- init file for Magit

;;; Commentary:

;; The best Git client, period.

;;; Code:

(straight-use-package
 '(git-commit :type git
              :host github
              :repo "magit/magit"
              :local-repo "magit"
              :files ("lisp/git-commit.el" "docs/AUTHORS.md" "LICENSE")))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-branch-prefer-remote-upstream '("master"))
  (magit-branch-adjust-remote-upstream-alist '(("origin/master" "upstream/master" "master"))))

(provide 'init-magit)

;;; init-magit.el ends here
