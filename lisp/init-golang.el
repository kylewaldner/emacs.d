;;; init-golang.el --- golang editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'go-mode)

(setq gofmt-command "goimports")


(add-hook 'purescript-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)))

(when (maybe-require-package 'lsp-mode)
  (add-hook 'go-mode-hook 'lsp))

(when (maybe-require-package 'lsp-ui)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))



(defun transform-mock-lines ()
  "Transform mock.On lines to mock.EXPECT() lines in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.On(\"\\([a-zA-Z_][a-zA-Z0-9_]*\\)\", \\([^\\)]+\\))\\.Return(\\([^\\)]+\\))"
            nil t)
      (replace-match "\\1.EXPECT().\\2(\\3).Return(\\4)" nil nil))))



(defun transform-mock-lines-in-project ()
  "Transform mock.On lines to mock.EXPECT() lines in all _test.go files in the current project."
  (interactive)
  (require 'projectile)
  (let ((files (projectile-current-project-files)))
    (dolist (file files)
      (when (string-suffix-p "_test.go" file)
        (with-current-buffer (find-file-noselect (expand-file-name file (projectile-project-root)))
          (transform-mock-lines)
          (save-buffer)
          (kill-buffer))))))

(provide 'init-golang)
;;; init-golang.el ends here
