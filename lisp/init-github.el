;;; init-github.el --- Github integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-git)

(straight-use-package 'yagist)
(when (straight-use-package 'bug-reference-github)
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode))

(straight-use-package 'github-clone)
;; Suppress cl deprecation warning for forge
(with-suppressed-warnings ((obsolete cl-lib))
  (straight-use-package 'forge))
(straight-use-package 'github-review)

(provide 'init-github)
;;; init-github.el ends here
