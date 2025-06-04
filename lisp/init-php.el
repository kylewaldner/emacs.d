;;; init-php.el --- Support for working with PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (straight-use-package 'php-mode)
  (straight-use-package 'smarty-mode)

  (when (straight-use-package 'company-php)
    (after-load 'company
      (push 'company-ac-php-backend company-backends))))

(provide 'init-php)
;;; init-php.el ends here
