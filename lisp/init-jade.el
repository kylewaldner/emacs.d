;;; init-jade.el --- jade support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'jade-mode)
  (add-hook 'jade-mode-hook 'linum-mode))

(provide 'init-jade)
;;; init-jade.el ends here
