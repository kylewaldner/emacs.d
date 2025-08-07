;;; init-yaml.el --- Support Yaml files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'yaml-mode)
(add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")
(add-hook 'yaml-mode-hook 'goto-address-prog-mode)

;; Indentation highlighting
(require 'highlight-indentation)
(add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)

(provide 'init-yaml)
;;; init-yaml.el ends here
