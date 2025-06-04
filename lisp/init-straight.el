;;; init-straight.el --- Settings and helpers for straight.el and use-package -*- lexical-binding: t -*-
;;; Commentary:
;; Modern package management with straight.el for parallel installation
;; and use-package for clean configuration.
;;; Code:

;; Bootstrap straight.el if not already installed
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         (or (bound-and-true-p straight-base-dir)
                             user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install and configure use-package
(straight-use-package 'use-package)

;; Install essential utilities before configuring use-package
(straight-use-package 'diminish)
(straight-use-package 'bind-key)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; Configure use-package behavior
(setq use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

;; Load fullframe for package list compatibility
(use-package fullframe
  :config
  (fullframe list-packages quit-window))

;; Essential packages loaded immediately
(use-package scratch)
(use-package command-log-mode)

;; COMPATIBILITY: Legacy functions for incremental migration
;; These functions allow gradual conversion from package.el style to use-package
;; Remove these once migration is complete

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE using straight.el.
MIN-VERSION and NO-REFRESH are ignored for compatibility.
This is a compatibility function - migrate to 'use-package' declarations."
  (straight-use-package package)
  t)

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE using straight.el, return non-nil if successful.
MIN-VERSION and NO-REFRESH are ignored for compatibility.
This is a compatibility function - migrate to 'use-package' declarations."
  (condition-case err
      (progn
        (straight-use-package package)
        t)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(provide 'init-straight)
;;; init-straight.el ends here
