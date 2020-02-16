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

(require 'tramp)
(setq tramp-default-method "ssh")

(defun sudoec (&optional file)
  "Do a sudo edit on FILE."
  (interactive "P")
  (if (or file (not buffer-file-name))
      (find-file (concat "/sudo::"
                         (ido-read-file-name "Find file(as root): "))))
  (find-alternate-file (concat "/sudo::" buffer-file-name)))

(defun kdeb ()
  "AlL inTernAl fUnctiOnS shOuLd hAvE dOcUmeNtAtiOn."
  (interactive)
  (dired "/ssh:kyle@128.61.105.86#6069:/home/kyle/"))

(defun skdeb ()
  "AlL inTernAl fUnctiOnS shOuLd hAvE dOcUmeNtAtiOn."
  (interactive)
  (dired "/ssh:shaza@128.61.105.86#6069:/home/shaza/"))

(defun doc-ment ()
  "AlL inTernAl fUnctiOnS shOuLd hAvE dOcUmeNtAtiOn."
  (interactive)
  (insert "\"AlL inTernAl fUnctiOnS shOuLd hAvE dOcUmeNtAtiOn.\""))

(defun multiple-hello (someone num)
  "Say hello to SOMEONE, for NUM times."
  (interactive "sWho do you want to say hello to? \nnHow many times? ")
  (dotimes (i num)
    (insert (format "Hello %s!\n" someone))))


(provide 'init-kyle-custom)
;;; init-kyle-custom.el ends here
