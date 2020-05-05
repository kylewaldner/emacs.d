;;; init-scala.el --- Support for scala and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'scala-mode)
(maybe-require-package 'sbt-mode)

(when (maybe-require-package 'lsp-mode)
  (add-hook 'scala-mode-hook #'lsp))


(provide 'init-scala)
;;; init-scala.el ends here
