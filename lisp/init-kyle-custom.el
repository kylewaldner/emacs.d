;;; init-kyle-custom.el --- all extra packages added by me - customizations still added in other files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; packages need to add
;; undo-tree, rainbow delimeters, auto-highlight-symbol, ace-jump-mode

;; other things, fix the window swtiching letter thing
;; less auto-complete delay

(when (straight-use-package 'multi-term)
  (setq multi-term-program "/bin/bash"))
;; TODO: get nice colors for multi term

;; (when (straight-use-package 'auto-highlight-symbol-mode)
;;   (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode))


(straight-use-package 'tern)

(when  (straight-use-package 'undo-tree)
  (global-undo-tree-mode)
  (diminish 'undo-tree-mode)
  ;; Configure undo-tree to store history files in ~/.emacs.d/undo-tree/
  (setq undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree" user-emacs-directory))))
  ;; Create the directory if it doesn't exist
  (let ((undo-tree-dir (expand-file-name "undo-tree" user-emacs-directory)))
    (unless (file-directory-p undo-tree-dir)
      (make-directory undo-tree-dir t)))
  ;; Enable auto-save of undo history
  (setq undo-tree-auto-save-history t)
  (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize)
  (define-key undo-tree-map (kbd "C-/") 'undo-tree-undo))


(defalias 'yes-or-no-p 'y-or-n-p)

;; no
;; (when  (straight-use-package 'dumb-jump)
;;   (dumb-jump-mode))


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

(defun read-lines (file-path)
  "Return a list of lines of a file at FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

;; hasck to make ag-ignore work better
(defvar kyle/ag-ignore-ready t)

(defun ag-ignore-setup ()
  "Setup hook for ag-ignore.  Insert contents of .agignore into minibuffer if running agignore."
  (if (and (eq this-command 'ag-ignore) kyle/ag-ignore-ready)
      (progn
        (setq kyle/ag-ignore-ready nil)
        (insert (mapconcat 'identity (read-lines "~/.agignore") ", "))
        )
    (setq kyle/ag-ignore-ready t)))

;; add special hook to minibuffer to improve ag-ignore
(add-hook 'minibuffer-setup-hook 'ag-ignore-setup)


(defun ag-ignore (input-string)
  "Write the list of file/directory names to the home .agignore.  INPUT-STRING."
  (interactive "slist of comma separated files/directories:")
  (write-region (mapconcat 'identity (split-string input-string ",\s?") "\n") nil "~/.agignore"))

(defun ag-clear-ignore ()
  "Clears the .agignore file."
  (interactive)
  (write-region "" nil "~/.agignore"))


;; global custom keys
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-x C-j") 'ace-jump-mode)
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
;; (add-hook 'before-save-hook 'delete-trailing-whitespace nil t) ;; try this if broken


;; open large files in fundamental mode
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

(setq redisplay-dont-pause t)

(setq warning-minimum-level :error)

;; (setq-default require-final-newline t)


;; move this stuff to remote.el

(defun tramp-sh-handle-vc-registered (f) nil)

;; (setq projectile-mode-line "Projectile")

(provide 'init-kyle-custom)
;;; init-kyle-custom.el ends here
