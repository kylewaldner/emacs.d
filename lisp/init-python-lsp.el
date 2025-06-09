;;; init-python-lsp.el --- Modern Python editing with LSP -*- lexical-binding: t -*-
;;; Commentary:
;; Modern Python development setup using LSP-mode with python-lsp-server (pylsp)
;; Replaces anaconda-mode with better Python 3.13 support
;; Uses unified LSP configuration from init-lsp-unified.el
;;; Code:

;; Load unified LSP configuration first
(require 'init-lsp-unified)

;; SConstruct/SConscript files are Python
(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

;; Python interpreter setup - use virtual environment if it exists
(let ((venv-python (expand-file-name "python-venv/bin/python" user-emacs-directory)))
  (when (file-exists-p venv-python)
    (setq python-shell-interpreter venv-python)
    ;; Set for LSP
    (setq lsp-pylsp-server-command `(,(expand-file-name "python-venv/bin/pylsp" user-emacs-directory))))
  ;; Fallback to system python3 if venv doesn't exist
  (unless (file-exists-p venv-python)
    (setq python-shell-interpreter "python3")
    (setq lsp-pylsp-server-command '("pylsp"))))

(setq python-shell-interpreter-args "-i --simple-prompt")
(setq python-indent-offset 4)

;; Python-specific LSP configuration
(with-eval-after-load 'lsp-mode
  ;; Python LSP configuration
  (setq lsp-pylsp-plugins-pylint-enabled nil        ; Use flake8 instead
        lsp-pylsp-plugins-flake8-enabled t
        lsp-pylsp-plugins-pycodestyle-enabled nil    ; Conflicts with flake8
        lsp-pylsp-plugins-autopep8-enabled t
        lsp-pylsp-plugins-yapf-enabled nil           ; Use autopep8
        lsp-pylsp-plugins-mccabe-enabled t
        lsp-pylsp-plugins-pyflakes-enabled nil       ; Use flake8 instead
        lsp-pylsp-plugins-jedi-completion-enabled t
        lsp-pylsp-plugins-jedi-hover-enabled t
        lsp-pylsp-plugins-jedi-references-enabled t
        lsp-pylsp-plugins-jedi-signature-help-enabled t
        lsp-pylsp-plugins-jedi-symbols-enabled t)

  ;; Hook LSP to Python mode
  (add-hook 'python-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (lsp-deferred)))))

;; Python-specific packages
(straight-use-package 'pip-requirements)

;; Indentation highlighting
(require 'highlight-indentation)
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)

;; Formatting with autopep8 (optional - LSP can handle this too)
(when (straight-use-package 'py-autopep8)
  ;; Uncomment to enable auto-formatting on save
  ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  )

;; Python-specific keybindings
(defun python-lsp-setup-keybindings ()
  "Set up Python-specific keybindings in addition to standard LSP bindings."
  (local-set-key (kbd "C-M-n") 'python-nav-up-list))

(add-hook 'python-mode-hook 'python-lsp-setup-keybindings)

;; Optional: Use dap-mode for debugging
(when (straight-use-package 'dap-mode)
  (require 'dap-python)
  (add-hook 'python-mode-hook 'dap-ui-mode)
  (add-hook 'python-mode-hook 'dap-mode))

(provide 'init-python-lsp)
;;; init-python-lsp.el ends here
