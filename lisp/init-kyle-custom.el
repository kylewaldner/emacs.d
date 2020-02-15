;;; init-kyle-custom --- all extra packages added by me - customizations still added in other files
;;; Commentary:
;;; Code:


;; packages need to add
;; undo-tree, rainbow delimeters, auto-highlight-symbol, ace-jump-mode

;; other things, fix the window swtiching letter thing
;; less auto-complete delay

(maybe-require-package 'rainbow-delimiters)


;; (when (maybe-require-package 'auto-highlight-symbol-mode)
;;   (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode))

(when (maybe-require-package 'ace-jump-mode)
  (global-set-key (kbd "C-x C-j") 'ace-jump-mode))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun kyle/load-init()
  "Reload `.emacs.d/init.el'."
  (interactive)
  (load-file "~/.emacs.d/init.el"))


(provide 'init-kyle-custom)
;;; init-kyle-custom.el ends here
