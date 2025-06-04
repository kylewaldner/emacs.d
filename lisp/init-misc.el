;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
(add-auto-mode 'tcl-mode "^Portfile\\'")
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-save-hook 'sanityinc/set-mode-for-new-scripts)

(defun sanityinc/set-mode-for-new-scripts ()
  "Invoke `normal-mode' if this file is a script and in `fundamental-mode'."
  (and
   (eq major-mode 'fundamental-mode)
   (>= (buffer-size) 2)
   (save-restriction
     (widen)
     (string= "#!" (buffer-substring (point-min) (+ 2 (point-min)))))
   (normal-mode)))


(when (maybe-require-package 'info-colors)
  (after-load 'info
    (add-hook 'Info-selection-hook 'info-colors-fontify-node)))


;; Handle the prompt pattern for the 1password command-line interface
(after-load 'comint
  (setq comint-password-prompt-regexp
        (concat
         comint-password-prompt-regexp
         "\\|^Please enter your password for user .*?:\\s *\\'")))



;; Replaced regex-tool with built-in rx macro for better regex building
;; Old: (when (maybe-require-package 'regex-tool)
;;        (setq-default regex-tool-backend 'perl))
;; Use built-in rx macro instead: (rx ...) or visual-regexp package
(maybe-require-package 'visual-regexp)
(maybe-require-package 'visual-regexp-steroids)
(global-set-key (kbd "C-c r") 'vr/replace)
(global-set-key (kbd "C-c q") 'vr/query-replace)

(after-load 're-builder
  ;; Support a slightly more idiomatic quit binding in re-builder
  (define-key reb-mode-map (kbd "C-c C-k") 'reb-quit))

(add-auto-mode 'conf-mode "^Procfile\\'")


(provide 'init-misc)
;;; init-misc.el ends here
