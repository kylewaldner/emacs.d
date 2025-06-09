;;; init-golang.el --- golang editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-lsp-unified)

;; Core Go Mode - the foundation
(when (straight-use-package 'go-mode)
  ;; File associations
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-dot-mod-mode))
  (add-to-list 'auto-mode-alist '("go\\.sum\\'" . go-dot-mod-mode))

  ;; Basic Go formatting setup
  (setq gofmt-command "goimports"  ; Use goimports for better import management
        gofmt-args '("-local" ""))  ; Will be set per-project if needed

  ;; Auto-format on save
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save nil t))))

;; Enhanced Go tooling integration
(when (straight-use-package 'go-eldoc)
  ;; Provides eldoc support for Go functions and variables
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;; Go code generation and manipulation tools
(when (straight-use-package 'go-add-tags)
  ;; Adds struct tags like json, yaml, etc.
  (with-eval-after-load 'go-mode
    (define-key go-mode-map (kbd "C-c t a") 'go-add-tags)
    (define-key go-mode-map (kbd "C-c t r") 'go-remove-tags)))

(when (straight-use-package 'go-fill-struct)
  ;; Auto-fills struct literals with field names
  (with-eval-after-load 'go-mode
    (define-key go-mode-map (kbd "C-c s f") 'go-fill-struct)))

(when (straight-use-package 'go-impl)
  ;; Generates method stubs for implementing interfaces
  (with-eval-after-load 'go-mode
    (define-key go-mode-map (kbd "C-c i i") 'go-impl)))

;; Go testing framework
(when (straight-use-package 'gotest)
  (with-eval-after-load 'go-mode
    ;; Test keybindings
    (define-key go-mode-map (kbd "C-c t t") 'go-test-current-test)
    (define-key go-mode-map (kbd "C-c t f") 'go-test-current-file)
    (define-key go-mode-map (kbd "C-c t p") 'go-test-current-project)
    (define-key go-mode-map (kbd "C-c t b") 'go-test-current-benchmark)
    (define-key go-mode-map (kbd "C-c t c") 'go-test-current-coverage)
    (define-key go-mode-map (kbd "C-c t x") 'go-run))

  ;; Test configuration
  (setq go-test-verbose t
        go-test-args "-v -count=1"))  ; Verbose output, no test caching

;; Go Playground integration for quick experiments
(when (straight-use-package 'go-playground)
  (with-eval-after-load 'go-mode
    (define-key go-mode-map (kbd "C-c p p") 'go-playground)
    (define-key go-mode-map (kbd "C-c p r") 'go-playground-rm)
    (define-key go-mode-map (kbd "C-c p d") 'go-playground-download)))

;; LSP configuration for Go with gopls
(with-eval-after-load 'lsp-mode
  ;; Configure gopls (Go's official language server)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "gopls")
    :major-modes '(go-mode go-dot-mod-mode)
    :priority 0
    :server-id 'gopls
    :library-folders-fn (lambda (_workspace) lsp-go-library-directories)))

  ;; gopls configuration
  (setq lsp-go-analyses '((fieldalignment . t)
                          (nilness . t)
                          (shadow . t)
                          (unusedparams . t)
                          (unusedwrite . t)
                          (useany . t)))

  ;; Enable additional gopls features
  (setq lsp-go-use-gofumpt t              ; Use gofumpt for formatting
        lsp-go-goimports-local ""         ; Set per-project
        lsp-go-build-flags []             ; Additional build flags
        lsp-go-env nil                    ; Environment variables
        lsp-go-directory-filters ["-node_modules" "-vendor"]
        lsp-go-hover-kind "FullDocumentation"
        lsp-go-link-target "pkg.go.dev"
        lsp-go-completion-documentation t
        lsp-go-use-placeholders t))

;; Enhanced company completion for Go
(with-eval-after-load 'company
  (add-hook 'go-mode-hook
            (lambda ()
              ;; Set completion backends specific to Go
              (set (make-local-variable 'company-backends)
                   '((company-capf company-dabbrev-code company-keywords)))
              (set (make-local-variable 'company-minimum-prefix-length) 1)
              (set (make-local-variable 'company-idle-delay) 0.1))))

;; Debugging support with DAP mode and Delve
(when (and (straight-use-package 'dap-mode)
           (executable-find "dlv"))
  (require 'dap-dlv-go)

  ;; DAP Go configuration
  (dap-register-debug-template
   "Go Debug"
   (list :type "go"
         :request "launch"
         :name "Launch Go Program"
         :mode "auto"
         :program nil
         :buildFlags nil
         :args nil
         :env nil
         :envFile nil))

  (dap-register-debug-template
   "Go Test"
   (list :type "go"
         :request "launch"
         :name "Go Test"
         :mode "test"
         :program nil
         :buildFlags nil
         :args nil))

  ;; Debug keybindings for Go
  (with-eval-after-load 'go-mode
    (define-key go-mode-map (kbd "C-c d d") 'dap-debug)
    (define-key go-mode-map (kbd "C-c d t") 'dap-debug-edit-template)
    (define-key go-mode-map (kbd "C-c d l") 'dap-debug-last)
    (define-key go-mode-map (kbd "C-c d r") 'dap-debug-recent)))

;; Project management with projectile integration
(when (featurep 'projectile)
  (defun go-project-setup ()
    "Set up Go-specific project configuration."
    (when (and (derived-mode-p 'go-mode)
               (projectile-project-root))
      (let* ((project-root (projectile-project-root))
             (go-mod-file (expand-file-name "go.mod" project-root)))
        (when (file-exists-p go-mod-file)
          ;; Set local goimports setting based on go.mod
          (with-temp-buffer
            (insert-file-contents go-mod-file)
            (goto-char (point-min))
            (when (re-search-forward "^module \\(.+\\)$" nil t)
              (let ((module-name (match-string 1)))
                (setq-local gofmt-args (list "-local" module-name)))))))))

  (add-hook 'go-mode-hook 'go-project-setup))

;; Go-specific snippet support
(when (straight-use-package 'yasnippet)
  (with-eval-after-load 'yasnippet
    ;; Add Go snippets directory if it exists
    (let ((go-snippets-dir (expand-file-name "snippets/go-mode" user-emacs-directory)))
      (when (file-directory-p go-snippets-dir)
        (add-to-list 'yas-snippet-dirs go-snippets-dir)))))

;; Enhanced syntax highlighting and indentation
(with-eval-after-load 'go-mode
  ;; Better indentation for Go
  (setq-default tab-width 4)
  (setq go-ts-mode-indent-offset 4)

  ;; Highlight important Go keywords
  (font-lock-add-keywords
   'go-mode
   '(("\\<\\(TODO\\|FIXME\\|NOTE\\|BUG\\|HACK\\):" 1 font-lock-warning-face t))))

;; LSP integration - enable for all Go files
(add-hook 'go-mode-hook 'lsp)
(add-hook 'go-dot-mod-mode-hook 'lsp)

;; Flycheck for additional static analysis
(when (straight-use-package 'flycheck)
  (add-hook 'go-mode-hook 'flycheck-mode)

  ;; Configure Go-specific flycheck checkers
  (with-eval-after-load 'flycheck
    ;; Use golangci-lint if available for additional checks
    (when (executable-find "golangci-lint")
      (flycheck-define-checker golangci-lint
        "A Go syntax and style checker using golangci-lint."
        :command ("golangci-lint" "run" "--out-format=checkstyle" source-original)
        :error-parser flycheck-parse-checkstyle
        :modes go-mode
        :predicate flycheck-buffer-saved-p)

      (add-to-list 'flycheck-checkers 'golangci-lint))))

;; Helpful Go utilities
(defun go-mod-tidy ()
  "Run go mod tidy in the current project."
  (interactive)
  (if-let ((project-root (projectile-project-root)))
      (let ((default-directory project-root))
        (async-shell-command "go mod tidy"))
    (message "Not in a Go project")))

(defun go-mod-vendor ()
  "Run go mod vendor in the current project."
  (interactive)
  (if-let ((project-root (projectile-project-root)))
      (let ((default-directory project-root))
        (async-shell-command "go mod vendor"))
    (message "Not in a Go project")))

(defun go-generate-project ()
  "Run go generate ./... in the current project."
  (interactive)
  (if-let ((project-root (projectile-project-root)))
      (let ((default-directory project-root))
        (async-shell-command "go generate ./..."))
    (message "Not in a Go project")))

;; Additional Go keybindings
(with-eval-after-load 'go-mode
  ;; Module management
  (define-key go-mode-map (kbd "C-c m t") 'go-mod-tidy)
  (define-key go-mode-map (kbd "C-c m v") 'go-mod-vendor)
  (define-key go-mode-map (kbd "C-c m g") 'go-generate-project)

  ;; Quick documentation lookup
  (define-key go-mode-map (kbd "C-c h d") 'godoc-at-point)

  ;; Import management
  (define-key go-mode-map (kbd "C-c i a") 'go-import-add)
  (define-key go-mode-map (kbd "C-c i r") 'go-remove-unused-imports))

;; Display helpful Go information
(defun go-show-project-info ()
  "Display information about the current Go project."
  (interactive)
  (if-let ((project-root (projectile-project-root)))
      (with-help-window "*Go Project Info*"
        (princ (format "Go Project Information\n"))
        (princ "======================\n\n")
        (princ (format "Project Root: %s\n" project-root))
        (let ((default-directory project-root))
          (princ (format "Go Version: %s\n"
                         (string-trim (shell-command-to-string "go version"))))
          (when (file-exists-p "go.mod")
            (princ (format "Module: %s\n"
                           (string-trim (shell-command-to-string "go list -m"))))
            (princ (format "Dependencies:\n%s\n"
                           (string-trim (shell-command-to-string "go list -m all | head -10")))))))
    (message "Not in a Go project")))

;; Hook for first-time setup
(defun go-setup-check ()
  "Check if Go development tools are properly installed."
  (let ((missing-tools '()))
    (unless (executable-find "go")
      (push "go" missing-tools))
    (unless (executable-find "gopls")
      (push "gopls" missing-tools))
    (unless (executable-find "goimports")
      (push "goimports" missing-tools))

    (when missing-tools
      (message "Missing Go tools: %s. Run: cd $HOME/.emacs.d/installation && ./install_go_deps.sh"
               (string-join missing-tools ", ")))))

(add-hook 'go-mode-hook 'go-setup-check)

;; Legacy mock transformation functions (keeping for compatibility)
(defun transform-go-mock-lines ()
  "Transform Go mock syntax from .On() style to .EXPECT() style, handling whitespace correctly."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([[:alnum:]_]+\\)\\.On(\"\\([[:alnum:]_]+\\)\"[[:space:]]*,[[:space:]]*" nil t)
      (replace-match "\\1.EXPECT().\\2(" t nil))))

(defun transform-go-mock-lines-in-project ()
  "Transform mock.On lines to mock.EXPECT() lines in all _test.go files in the current project."
  (interactive)
  (require 'projectile)
  (let ((files (projectile-current-project-files)))
    (dolist (file files)
      (when (string-suffix-p "_test.go" file)
        (with-current-buffer (find-file-noselect (expand-file-name file (projectile-project-root)))
          (transform-go-mock-lines)
          (save-buffer)
          (kill-buffer))))))

(provide 'init-golang)
;;; init-golang.el ends here
