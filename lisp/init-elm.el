;;; init-elm.el --- Support for the Elm language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (straight-use-package 'elm-mode)
  (after-load 'elm-mode
    (diminish 'elm-indent-mode)
    (after-load 'company
      (push 'company-elm company-backends))
    (when (executable-find "elm-format")
      (setq-default elm-format-on-save t)))
  (straight-use-package 'elm-test-runner)
  (when (straight-use-package 'flycheck-elm)
    (after-load 'elm-mode
      (flycheck-elm-setup)))
  (when (straight-use-package 'add-node-modules-path)
    (add-hook 'elm-mode-hook 'add-node-modules-path)))

(provide 'init-elm)
;;; init-elm.el ends here
