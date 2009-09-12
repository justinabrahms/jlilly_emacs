;; Add in git version control hooks
(require 'vc-git)
(when (featurep 'vcgit) (add-to-list 'vc-handled-backends 'git))
(require 'git)
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for git" t)

(require 'magit)
(autoload 'magit-status "magit" nil t) ;; autoload magit-status for magit mode
(require 'egg) ;; will need to choose between this one and magit, I think.
