;;; init-scala.el --- Support for scala and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'scala-mode)

(provide 'init-scala)
;;; init-scala.el ends here

(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

(when (maybe-require-package 'sbt-mode)
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(when (maybe-require-package 'lsp-mode)
  (add-hook 'scala-mode-hook 'lsp-mode)
  (add-hook 'lsp-mode-hook 'lsp-lens-mode)
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  ;;       (setq gc-cons-threshold 100000000) ;; 100mb
  ;;       (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;;       (setq lsp-idle-delay 0.500)
  ;;       (setq lsp-log-io nil)
  ;;       (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil)
  )

(when (maybe-require-package 'lsp-metals)
  (setq lsp-metals-treeview-show-when-views-received t)
  )

(maybe-require-package 'lsp-ui)
;; (maybe-require-package 'company-lsp) ;; outdated

(when (maybe-require-package 'dap-mode)
  (add-hook 'lsp-mode-hook 'dap-mode)
  (add-hook 'lsp-mode-hook 'dap-ui-mode))

;; now need to build the metals emacs binary, and make sure it is available on $PATH
;; https://scalameta.org/metals/docs/editors/emacs.html

(provide 'init-scala)
;;; init-scala.el ends here
