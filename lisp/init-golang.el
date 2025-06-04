;;; init-golang.el --- golang editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'go-mode)

(setq gofmt-command "goimports")


(add-hook 'purescript-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)))

(when (straight-use-package 'lsp-mode)
  (add-hook 'go-mode-hook 'lsp))

(when (straight-use-package 'lsp-ui)
  (add-hook 'go-mode-hook 'flycheck-mode))



(defun transform-mock-lines-old ()
  "Transform mock.On lines to mock.EXPECT() lines in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.On(\"\\([a-zA-Z_][a-zA-Z0-9_]*\\)\", \\([^\\)]+\\))\\.Return(\\([^\\)]+\\))"
            nil t)
      (replace-match "\\1.EXPECT().\\2(\\3).Return(\\4)" nil nil))))


(defun transform-mock-lines-less-old ()
  "Transform mock.On lines to mock.EXPECT() lines in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.On(\"\\([a-zA-Z_][a-zA-Z0-9_]*\\)\",[[:space:]]*\\([^)]*\\))[[:space:]]*\\.[[:space:]]*Return(\\([^)]*\\))"
            nil t)
      (replace-match "\\1.EXPECT().\\2(\\3).Return(\\4)" nil nil))))

(defun transform-go-mock-lines ()
  "Transform Go mock syntax from .On() style to .EXPECT() style, handling whitespace correctly."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([[:alnum:]_]+\\)\\.On(\"\\([[:alnum:]_]+\\)\"[[:space:]]*,[[:space:]]*" nil t)
      (replace-match "\\1.EXPECT().\\2(" t nil))))



(defun transform-go-mock-lines-in-project ()
  "Transform mock.On lines to mock.EXPECT() lines in all _test.go files in the current project."
  (interactive)
  (require 'projectile)
  (let ((files (projectile-current-project-files)))
    (dolist (file files)
      (when (string-suffix-p "_test.go" file)
        (with-current-buffer (find-file-noselect (expand-file-name file (projectile-project-root)))
          (transform-go-mock-lines)
          (save-buffer)
          (kill-buffer))))))

(provide 'init-golang)
;;; init-golang.el ends here
