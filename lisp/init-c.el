;;; init-c.el --- Support for C and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (add-hook 'c-mode-hook 'lsp)

;; (add-hook 'cpp-mode-hook 'lsp)

;; (maybe-require-package 'lsp-treemacs)

;; ;; (setq gc-cons-threshold (* 100 1024 1024)
;; ;;       read-process-output-max (*1024 1024)
;; ;;       treemacs-space-between-root-nodes nil
;; ;;       lsp-idle-delay 0.1
;; ;;       lsp-headerline-breadcrumb-enable t)

;; (setq lsp-idle-delay 0)

;; (with-eval-after-load 'lsp-mode-hook
;;   add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration
;;   (require 'dap-cpptools))


;;; all that stuff does turn on lsp, but lsp is no better than just flycheck...


;; all this stuff should work for irony

(when (maybe-require-package 'irony)
  ;; (unless (irony-find-server-executable) (call-interactively #'irony-install-server))
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (setq-default irony-cdb-complilation-databases '(irony-cdb-libclang
                                                   irony-cdb-clang-complete))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(when (maybe-require-package 'company-irony)
  (eval-after-load 'company '(add-to-list 'company-backends 'company-irony)))

(when (maybe-require-package 'flycheck-irony)
  (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(when (maybe-require-package 'irony-eldoc)
  (add-hook 'irony-mode-hook #'irony-eldoc))

;;;;;;;;;;;;;;;;;;;

;; (when (maybe-require-package 'rtags)
;; ;;  (unless (rtags-executable-find "rc") (error "Binary rc is not installed!"))
;; ;;  (unless (rtags-executable-find "rdm") (error "Binary rdm is not installed!"))

;; ;;  (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
;; ;;  (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
;; ;;  (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
;;   (rtags-enable-standard-keybindings)
;;   (add-hook 'kill-emacs-hook 'rtags-quit-rdm))

;; (when (maybe-require-package 'company-rtags)
;;   (setq rtags-autostart-diagnostics t)
;;   (rtags-diagnostics)
;;   (setq rtags-completions-enabled t)
;;   (eval-after-load 'company '(add-to-list 'company-backends 'company-rtags)))

;; (when (maybe-require-package 'flycheck-rtags)
;;   ;; ensure that we use only rtags checking
;;   ;; https://github.com/Andersbakken/rtags#optional-1
;;   (defun setup-flycheck-rtags ()
;;     (flycheck-select-checker 'rtags)
;;     (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;     (setq-local flycheck-check-syntax-automatically nil)
;;     (rtags-set-periodic-reparse-timeout 2.0)  ;; Run flycheck 2 seconds after being idle.
;;     )
;;   (add-hook 'c-mode-hook #'setup-flycheck-rtags)
;;   (add-hook 'c++-mode-hook #'setup-flycheck-rtags))

(provide 'init-c)
;;; init-c.el ends here
