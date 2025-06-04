;;; init-rust.el --- Support for the Rust language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (straight-use-package 'rust-mode)
  (when (straight-use-package 'racer)
    (add-hook 'rust-mode-hook #'racer-mode))
  (when (straight-use-package 'company)
    (add-hook 'racer-mode-hook #'company-mode)))

(when (straight-use-package 'flycheck-rust)
  (after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(provide 'init-rust)
;;; init-rust.el ends here
