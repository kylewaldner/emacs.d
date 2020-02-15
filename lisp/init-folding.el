;;; init-folding.el --- Support code and region folding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'origami)
  (after-load 'origami
    (define-key origami-mode-map (kbd "C-c C-a") 'origami-recursively-toggle-node)
    (define-key origami-mode-map (kbd "C-c C-A") 'origami-toggle-all-nodes))
  (add-hook 'prog-mode-hook 'origami-mode))


(provide 'init-folding)
;;; init-folding.el ends here
