;;; init-graphql.el --- Support for graphql -*- lexical-binding: t -*-
;;; Commentary:
;;; works with graphql
;;; Code:

(straight-use-package 'graphql-mode)

(add-to-list 'auto-mode-alist '("\\.graphql\\'" . graphql-mode))
(add-to-list 'auto-mode-alist '("\\.gql\\'" . graphql-mode))
(add-to-list 'auto-mode-alist '("\\.graphqls\\'" . graphql-mode))

(setq graphql-indent-level 2)

(add-hook 'graphql-mode-hook #'electric-pair-mode)
(add-hook 'graphql-mode-hook #'company-mode)

(with-eval-after-load 'flycheck
  (add-hook 'graphql-mode-hook #'flycheck-mode))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(graphql-mode . "graphql"))
  (add-hook 'graphql-mode-hook #'lsp-deferred))


(provide 'init-graphql)
;;; init-graphql.el ends here
