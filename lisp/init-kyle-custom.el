;;; init-kyle-custom.el --- all extra packages added by me - customizations still added in other files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; packages need to add
;; undo-tree, rainbow delimeters, auto-highlight-symbol, ace-jump-mode

;; other things, fix the window swtiching letter thing
;; less auto-complete delay

(maybe-require-package 'rainbow-delimiters)

(when (maybe-require-package 'multi-term)
  (setq multi-term-program "/bin/bash"))
;; TODO: get nice colors for multi term

;; (when (maybe-require-package 'auto-highlight-symbol-mode)
;;   (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode))

(maybe-require-package 'restclient)

(maybe-require-package 'ace-jump-mode)

(maybe-require-package 'tern)

(when  (maybe-require-package 'undo-tree)
  (global-undo-tree-mode)
  (diminish 'undo-tree-mode)
  (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize)
  (define-key undo-tree-map (kbd "C-/") 'undo-tree-undo))


(defalias 'yes-or-no-p 'y-or-n-p)

(when  (maybe-require-package 'dumb-jump)
  (dumb-jump-mode))


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

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun comment-or-uncomment-region-or-line-and-jump ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (comment-or-uncomment-region-or-line)
  (if (region-active-p)
      ;; (message "end of region is %d" (region-end))
      (goto-char (region-end)))
  (forward-line))


;; global custom keys
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-j") 'ace-jump-mode)
    ;; hack since idk how to reset tern mode keybindings
    (define-key map (kbd "C-c t r") 'tern-rename-variable)
    (define-key map (kbd "C-c t t") 'tern-get-type)
    (define-key map (kbd "C-c t d") 'tern-get-docs)
    (define-key map (kbd "C-c t f") 'tern-find-definition)
    (define-key map (kbd "C-c t b") 'tern-pop-find-definition)
    (define-key map (kbd "C-'") 'comment-or-uncomment-region-or-line)
    (define-key map (kbd "C-c '") 'comment-or-uncomment-region-or-line-and-jump)
    map)
  "The my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

(diminish 'my-keys-minor-mode)


(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init-kyle-custom)
;;; init-kyle-custom.el ends here
