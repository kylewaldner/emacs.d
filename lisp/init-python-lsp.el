;;; init-python-lsp.el --- Modern Python editing with LSP -*- lexical-binding: t -*-
;;; Commentary:
;; Modern Python development setup using LSP-mode with python-lsp-server (pylsp)
;; Replaces anaconda-mode with better Python 3.13 support
;;; Code:

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

;; Required packages
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'company)
(straight-use-package 'flycheck)
(straight-use-package 'pip-requirements)

;; LSP-mode configuration
(when (straight-use-package 'lsp-mode)
  (setq lsp-keymap-prefix "C-c l")
  
  ;; Performance optimizations
  (setq lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-completion-provider :company-capf
        lsp-headerline-breadcrumb-enable nil)
  
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

;; LSP UI enhancements
(when (straight-use-package 'lsp-ui)
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.5
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics nil  ; Disable diagnostics in sideline
        lsp-ui-sideline-show-code-actions t   ; Show code actions in sideline
        lsp-ui-sideline-update-mode 'line     ; Update sideline when cursor moves
        lsp-ui-sideline-delay 0.2             ; Delay before showing sideline
        lsp-ui-peek-enable t)
  
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; Company completion
(when (straight-use-package 'company)
  (add-hook 'python-mode-hook 'company-mode)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.2))

;; Flycheck for syntax checking
(when (straight-use-package 'flycheck)
  (add-hook 'python-mode-hook 'flycheck-mode)
  ;; LSP provides diagnostics, but we can also enable flycheck checkers as backup
  (with-eval-after-load 'flycheck
    (with-eval-after-load 'lsp-mode
      ;; Disable flycheck checkers that conflict with LSP
      (setq-default flycheck-disabled-checkers '(python-flake8 python-pylint python-pycompile))
      ;; Configure flycheck to work alongside LSP
      (setq flycheck-indication-mode 'left-fringe  ; Show indicators in left fringe
            flycheck-highlighting-mode 'lines)     ; Highlight entire lines with errors
      ;; Don't disable flycheck completely, let it show LSP diagnostics
      ;; (add-hook 'python-mode-hook
      ;;           (lambda ()
      ;;             (when (and (bound-and-true-p lsp-mode)
      ;;                        (lsp-workspaces))
      ;;               ;; Prefer LSP diagnostics over flycheck in LSP-managed buffers
      ;;               (flycheck-mode -1))))
      )))

;; Indentation highlighting
(require 'highlight-indentation)
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)

;; Formatting with autopep8 (optional - LSP can handle this too)
(when (straight-use-package 'py-autopep8)
  ;; Uncomment to enable auto-formatting on save
  ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  )

;; Additional Python mode settings
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-M-n") 'python-nav-up-list)
            ;; LSP keybindings
            (local-set-key (kbd "M-.") 'lsp-find-definition)
            (local-set-key (kbd "M-?") 'lsp-find-references)
            (local-set-key (kbd "C-c C-d") 'lsp-describe-thing-at-point)
            (local-set-key (kbd "C-c C-r") 'lsp-rename)))

;; Optional: Use dap-mode for debugging
(when (straight-use-package 'dap-mode)
  (require 'dap-python)
  (add-hook 'python-mode-hook 'dap-ui-mode)
  (add-hook 'python-mode-hook 'dap-mode))

(provide 'init-python-lsp)
;;; init-python-lsp.el ends here 