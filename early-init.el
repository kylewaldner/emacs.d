;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.
;; Now also includes straight.el bootstrap for parallel package management.

;;; Code:

;; Suppress package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Performance optimizations for startup
(setq gc-cons-threshold most-positive-fixnum ; Maximize GC threshold during init
      gc-cons-percentage 0.6)

;; Reset GC after initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))

;; Prevent unwanted runtime compilation for gccemacs users
(setq comp-deferred-compilation nil)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight.el for optimal performance
(setq straight-use-package-by-default t
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1)

;; Suppress deprecated cl package warnings from legacy packages
;; This prevents "Package cl is deprecated" warnings that occur when
;; third-party packages still use the old cl library
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((obsolete)))
(setq warning-suppress-types '((obsolete)))

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
