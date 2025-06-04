;;; init-straight.el --- Settings and helpers for straight.el and use-package -*- lexical-binding: t -*-
;;; Commentary:
;; Modern package management with straight.el for parallel installation
;; and use-package for clean configuration.
;;; Code:

;; Install and configure use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

;; Essential utilities that need to be loaded early
(use-package diminish
  :demand t)

(use-package bind-key
  :demand t)

;; Compatibility functions for migration from package.el
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE using straight.el.
MIN-VERSION and NO-REFRESH are ignored for compatibility."
  (straight-use-package package)
  t)

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE using straight.el, return non-nil if successful.
MIN-VERSION and NO-REFRESH are ignored for compatibility."
  (condition-case err
      (progn
        (straight-use-package package)
        t)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

;; Load fullframe for package list compatibility
(use-package fullframe
  :config
  (fullframe list-packages quit-window))

;; Essential packages loaded immediately
(use-package scratch)
(use-package command-log-mode)

(provide 'init-straight)
;;; init-straight.el ends here
