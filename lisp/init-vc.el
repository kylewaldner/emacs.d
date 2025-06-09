;;; init-vc.el --- Version control support -*- lexical-binding: t -*-
;;; Commentary:

;; Most version control packages are configured separately: see
;; init-git.el, for example.

;;; Code:

(when (straight-use-package 'diff-hl)
  (require 'diff-hl)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  ;; Enable global diff-hl mode
  (global-diff-hl-mode)

  (define-key diff-hl-mode-map
              (kbd "<left-fringe> <mouse-1>")
              'diff-hl-diff-goto-hunk))

(straight-use-package 'browse-at-remote)

(provide 'init-vc)
;;; init-vc.el ends here
