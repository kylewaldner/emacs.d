;;; init-cpp.el --- Support for C and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; http://blog.lujun9972.win/emacs-document/blog/2018/03/22/emacs-as-a-c++-ide/index.html


;;  If you are using Cmake to build your project, it is really easy to generate compilation database, since CMake has support for it. You just provide CMake with correct flag (cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ...) and that is it! Later you can just call cmake . and it will update compilation database if needed. This is what I used for my project, since it uses CMake.

(defun kyle/c-indent (indent-amount)
  "set INDENT-AMOUNT level for c modes."
  (interactive "sC indent value:")
  (setq c-basic-offset (string-to-number indent-amount)))

(when (straight-use-package 'irony)
  ;; If irony server was never installed, install it.
  (require 'irony)
  (unless (irony--find-server-executable) (call-interactively #'irony-install-server))
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)

  ;; Use compilation database first, clang_complete as fallback.
  (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
                                                  irony-cdb-clang-complete))

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (when (and (straight-use-package 'company-irony) (straight-use-package 'company-irony-c-headers))
    (eval-after-load 'company '(add-to-list 'company-backends '(company-irony-c-headers company-irony))))


  (when (straight-use-package 'flycheck-irony)
    (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

  (when (straight-use-package 'irony-eldoc)
    (add-hook 'irony-mode-hook #'irony-eldoc))
  )

;; (when (straight-use-package 'cmake-ide)
;;   (cmake-ide-setup))

(straight-use-package 'cmake-mode)
(straight-use-package 'cmake-font-lock)

(setq kyle/use-rtags nil)

;; (require 'cc-mode)

;; (straight-use-package 'rtags)

;; (when (require 'rtags)
;;   ;; (unless (rtags-executable-find "rc") (error "Binary rc is not installed"))
;;   ;; (unless (rtags-executable-find "rc") (error "Binary rc is not installed"))
;;   (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
;;   (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
;;   (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
;;   (rtags-enable-standard-keybindings)
;;   (when (straight-use-package 'company-rtags)
;;     (setq rtags-autostart-diagnostics t)
;;     (rtags-diagnostics)
;;     (setq rtags-completions-enabled t)
;;     (eval-after-load 'company '(add-to-list 'company-backends 'company-rtags))
;;     )
;;   )


;; (when (and (straight-use-package 'flycheck-rtags) kyle/use-rtags)
;;   ;; ensure that we use only rtags checking
;;   ;; https://github.com/Andersbakken/rtags#optional-1
;;   (defun setup-flycheck-rtags ()
;;     (flycheck-select-checker 'rtags)
;;     (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;     (setq-local flycheck-check-syntax-automatically nil)
;;     (rtags-set-periodic-reparse-timeout 2.0) ;; Run flycheck 2 seconds after being idle.
;;     )
;;   (add-hook 'c-mode-hook #'setup-flycheck-rtags)
;;   (add-hook 'c++-mode-hook #'setup-flycheck-rtags))


;; (defun kyle/c-project-setup ()
;;   "Setup a the c project by generating compile commands with bear."
;;   (interactive)
;;   (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
;;     (shell-command "bear make"))
;;   (call-interactively 'irony-cdb-json-add-compile-commands-path)
;;   ;; (call-interactively 'irony-cdb-json-select)
;;   )


(provide 'init-cpp)
;;; init-cpp.el ends here
