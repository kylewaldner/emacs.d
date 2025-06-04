;;; init-nix.el --- Support for the Nix package manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'nix-mode)
(straight-use-package 'nix-sandbox)
(straight-use-package 'nix-buffer)

(when (straight-use-package 'nixos-options)
  (when (straight-use-package 'company-nixos-options)
    (after-load 'company

      ;; Patch pending https://github.com/travisbhartwell/nix-emacs/pull/46
      (after-load 'company-nixos-options
        (defun company-nixos--in-nix-context-p ()
          (unless (executable-find "nix-build")
            (or (derived-mode-p 'nix-mode 'nix-repl-mode)
                (let ((file-name (buffer-file-name (current-buffer))))
                  (and file-name (equal "nix" (file-name-extension file-name))))))))

      (add-to-list 'company-backends 'company-nixos-options))))



(provide 'init-nix)
;;; init-nix.el ends here
