;;; init-textile.el --- Edit Textile markup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (straight-use-package 'textile-mode)
  (setq auto-mode-alist
        (cons '("\\.textile\\'" . textile-mode) auto-mode-alist)))


(provide 'init-textile)
;;; init-textile.el ends here
