;;; init-workbench.el --- Workbench worktree manager -*- lexical-binding: t -*-
;;; Commentary:
;; Install and configure workbench.el from kylewaldner02/workbench
;;; Code:

(straight-use-package
 '(workbench :type git
             :host github
             :repo "kylewaldner02/workbench"
             :files ("elisp/workbench.el")))

(with-eval-after-load 'workbench
  (setq workbench-open-git-function #'kyle-git-opener-workflow)
  (setq workbench-open-terminal-function #'kyle-terminal-workflow)
  (setq workbench-open-claude-function #'kyle-claude-workflow)
  (setq workbench-new-session-function #'kyle-new-session-workflow)
  (setq workbench-resume-session-function #'kyle-resume-session-workflow)
  (setq workbench-fork-session-function #'kyle-fork-session-workflow)
  (setq workbench-branch-strip-prefixes '("kylewaldner/" "kyle/" "kylewaldner02/")))

(provide 'init-workbench)
;;; init-workbench.el ends here
