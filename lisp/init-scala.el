;;; init-scala.el --- Support for scala and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'scala-mode)
(maybe-require-package 'sbt-mode)

(when (maybe-require-package 'lsp-mode)
  (add-hook 'scala-mode-hook #'lsp)
  ;; (add-hook 'lsp-mode-hook 'lsp-lens-mode) ;; // TODO: figure out what this is for
  (setq lsp-prefer-flymake nil))

(when (maybe-require-package 'lsp-metals)
  (setq lsp-metals-treeview-show-when-views-received t))

(maybe-require-package 'lsp-ui)

(when (maybe-require-package 'company-lsp)
  (after-load 'company
    (add-to-list 'company-backends 'company-lsp)))

(after-load 'dap-mode
  (add-hook lsp-mode-hook 'dap-mode)
  (add-hook lsp-mode-hook 'dap-ui-mode))

;; curl -L -o coursier https://git.io/coursier-cli
;; chmod +x coursier
;; ./coursier bootstrap \
;; --java-opt -Xss4m \
;; --java-opt -Xms100m \
;; --java-opt -Dmetals.client=emacs \
;; org.scalameta:metals_2.12:0.9.3 \
;; -r bintray:scalacenter/releases \
;; -r sonatype:snapshots \
;; -o /usr/local/bin/metals-emacs -f

(provide 'init-scala)
;;; init-scala.el ends here
