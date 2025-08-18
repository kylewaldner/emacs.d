;;; init-golang.el --- golang editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-lsp-unified)

;; Ensure flycheck is available for our checker definition
(require 'init-flycheck)
(require 'flycheck)

;; Add asdf shims to exec-path if it exists
(let ((asdf-shims (expand-file-name "~/.asdf/shims")))
  (when (file-directory-p asdf-shims)
    (add-to-list 'exec-path asdf-shims)
    (setenv "PATH" (concat asdf-shims ":" (getenv "PATH")))))

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

;; Override lsp-go settings BEFORE it loads to prevent deprecated settings
;; This must run before lsp-mode loads lsp-go
(defun init-go-remove-deprecated-settings ()
  "Remove deprecated gopls settings that cause warnings."
  (when (boundp 'lsp-go-analysis-progress-reporting)
    (setq lsp-go-analysis-progress-reporting nil))
  (when (boundp 'lsp-go-complete-function-calls)
    (setq lsp-go-complete-function-calls nil))
  (when (boundp 'lsp-go-standalone-tags)
    (setq lsp-go-standalone-tags nil))
  (when (boundp 'lsp-go-symbol-scope)
    (setq lsp-go-symbol-scope nil)))

;; Run this early, before lsp-mode loads
(add-hook 'after-init-hook #'init-go-remove-deprecated-settings -100)

;; Also run when go-mode starts, before LSP
(add-hook 'go-mode-hook #'init-go-remove-deprecated-settings -100)

;; Explicitly set the correct gopls path to use the updated version
(with-eval-after-load 'lsp-go
  ;; Use the gopls from GOPATH instead of the old homebrew version
  ;; This dynamically constructs the path based on GOPATH
  ;; When GOPATH is not set, Go defaults to $HOME/go
  (let* ((gopath (or (getenv "GOPATH")
                     (expand-file-name "~/go")))  ; Go's default when GOPATH not set
         (gopls-path (expand-file-name "bin/gopls" gopath)))
    (when (file-executable-p gopls-path)
      (setq lsp-go-gopls-server-path gopls-path)
      (message "Using gopls from: %s" gopls-path))))

;; Remove deprecated settings from lsp-configuration after lsp-go loads
(with-eval-after-load 'lsp-go
  ;; Force the deprecated settings to be nil
  (setq lsp-go-analysis-progress-reporting nil)
  (setq lsp-go-complete-function-calls nil)
  (setq lsp-go-standalone-tags nil)
  (setq lsp-go-symbol-scope nil)

  ;; Remove them from the registered custom settings
  (when (boundp 'lsp-configuration)
    (let ((config lsp-configuration))
      (when (and config (hash-table-p config))
        ;; Remove the deprecated settings from the configuration
        (remhash "gopls.analysisProgressReporting" config)
        (remhash "gopls.completeFunctionCalls" config)
        (remhash "gopls.standaloneTags" config)
        (remhash "gopls.symbolScope" config)))))

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

  ;; Modern gopls configuration using lsp-register-custom-settings
  ;; We use lsp-register-custom-settings for all gopls settings
  (lsp-register-custom-settings
   '(;; Analysis settings
     ("gopls.analyses.nilness" t t)
     ("gopls.analyses.shadow" t t)
     ("gopls.analyses.unusedparams" t t)
     ("gopls.analyses.unusedwrite" t t)
     ("gopls.analyses.useany" t t)
     ;; Enable staticcheck analyzers for better error detection
     ("gopls.staticcheck" t t)
     ;; Additional useful analyzers
     ("gopls.analyses.unreachable" t t)
     ("gopls.analyses.unusedresult" t t)
     ;; Completion and formatting
     ("gopls.usePlaceholders" t t)
     ("gopls.gofumpt" t t)
     ;; UI settings
     ("gopls.semanticTokens" t t)
     ;; Inlay hints
     ("gopls.hints.assignVariableTypes" t t)
     ("gopls.hints.compositeLiteralFields" t t)
     ("gopls.hints.compositeLiteralTypes" t t)
     ("gopls.hints.constantValues" t t)
     ("gopls.hints.functionTypeParameters" t t)
     ("gopls.hints.parameterNames" t t)
     ("gopls.hints.rangeVariableTypes" t t)))

  ;; Workspace directory filters to reduce noise
  (setq lsp-go-directory-filters ["-**/vendor" "-**/node_modules" "-**/.git" "-**/build" "-**/dist" "-**/bin"])

  ;; Don't watch library directories
  (setq lsp-go-library-directories '()))

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

;; Flycheck configuration for Go - using individual linters instead of golangci-lint
;; The built-in Go checkers will run in this order:
;; 1. go-gofmt (formatting)
;; 2. go-vet (suspicious code)
;; 3. go-build or go-test (syntax and types)
;; 4. go-errcheck (unhandled errors)
;; 5. go-unconvert (unnecessary type conversions)
;; 6. go-staticcheck (static analysis)

;; Setup function for go-mode with individual linters
(defun my-go-mode-flycheck-setup ()
  "Setup flycheck for Go mode with individual linters."
  (when (derived-mode-p 'go-mode)
    ;; Enable flycheck
    (flycheck-mode 1)
    ;; Make sure all individual Go checkers are enabled (not disabled)
    (setq-local flycheck-disabled-checkers
                (remove 'go-gofmt
                        (remove 'go-vet
                                (remove 'go-build
                                        (remove 'go-test
                                                (remove 'go-errcheck
                                                        (remove 'go-unconvert
                                                                (remove 'go-staticcheck
                                                                        flycheck-disabled-checkers))))))))
    ;; Configure specific linter options if needed
    (when (boundp 'flycheck-go-vet-print-functions)
      (setq-local flycheck-go-vet-print-functions nil))
    ;; Ensure build tags are available for all checkers that need them
    (when (and (boundp 'flycheck-go-build-tags)
               (projectile-project-root))
      ;; You can customize build tags per project if needed
      ;; (setq-local flycheck-go-build-tags '("integration" "unit"))
      )
    ;; Install dependencies for go-build/go-test if needed
    (setq-local flycheck-go-build-install-deps nil)
    ;; Set Go version for staticcheck if available
    (when (executable-find "staticcheck")
      (setq-local flycheck-go-version nil)) ;; nil means use default version
    ;; Force errcheck and unconvert to be available
    (add-to-list 'flycheck-checkers 'go-errcheck)
    (add-to-list 'flycheck-checkers 'go-unconvert)
    ;; Start syntax checking after a short delay
    (run-with-timer 0.5 nil 'flycheck-buffer)))

;; Add hook for go-mode
(add-hook 'go-mode-hook #'my-go-mode-flycheck-setup)

;; Configure the checker chain to ensure all linters run
(with-eval-after-load 'flycheck
  ;; Clear any existing next-checkers to start fresh
  (setq flycheck-checkers
        (delq 'go-golint flycheck-checkers))  ; Remove deprecated go-golint

  ;; Function to reset and configure Go checker chains
  (defun configure-go-checker-chain ()
    ;; Reset all next-checkers for Go checkers
    (dolist (checker '(go-gofmt go-vet go-build go-test go-errcheck go-unconvert go-staticcheck))
      (put checker 'flycheck-next-checkers nil))

    ;; Set up the complete checker chain for Go
    ;; go-gofmt -> go-vet -> go-build -> multiple parallel checkers
    (flycheck-add-next-checker 'go-gofmt 'go-vet)
    (flycheck-add-next-checker 'go-vet 'go-build)
    ;; After go-build, run multiple checkers
    (flycheck-add-next-checker 'go-build 'go-errcheck)
    (flycheck-add-next-checker 'go-build 'go-unconvert 'append)
    (flycheck-add-next-checker 'go-build 'go-staticcheck 'append)

    ;; For test files, similar setup after go-test
    (flycheck-add-next-checker 'go-test 'go-errcheck)
    (flycheck-add-next-checker 'go-test 'go-unconvert 'append)
    (flycheck-add-next-checker 'go-test 'go-staticcheck 'append))

  ;; Configure the chains
  (configure-go-checker-chain)

  ;; Also run configuration when Go mode is loaded
  (add-hook 'go-mode-hook #'configure-go-checker-chain)

  ;; Redefine go-errcheck and go-unconvert to ensure they work properly
  ;; First remove them if they exist
  (setq flycheck-checkers (remove 'go-errcheck flycheck-checkers))
  (setq flycheck-checkers (remove 'go-unconvert flycheck-checkers))

  ;; Define go-errcheck
  (flycheck-define-checker go-errcheck
    "Check for unchecked errors in Go code using errcheck.
See URL `https://github.com/kisielk/errcheck'."
    :command ("errcheck" "./...")
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":" "\t" (message) line-end))
    :modes go-mode
    :predicate flycheck-buffer-saved-p)

  ;; Define go-unconvert
  (flycheck-define-checker go-unconvert
    "Check for unnecessary type conversions in Go code using unconvert.
See URL `https://github.com/mdempsky/unconvert'."
    :command ("unconvert" "./...")
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": " (message) line-end))
    :modes go-mode
    :predicate flycheck-buffer-saved-p)

  ;; Add them back to the checkers list
  (add-to-list 'flycheck-checkers 'go-errcheck)
  (add-to-list 'flycheck-checkers 'go-unconvert))

;; Helpful Go utilities
(defun go-find-replace-modules ()
  "Find local module replacements in go.mod for file watching.
Returns a list of local directory paths from replace directives."
  (let ((go-mod (locate-dominating-file default-directory "go.mod"))
        (replacements '()))
    (when go-mod
      (with-temp-buffer
        (insert-file-contents (expand-file-name "go.mod" go-mod))
        (goto-char (point-min))
        (while (re-search-forward "^\\s-*replace\\s-+[^=]+=>\\s-+\\(\\.\\.[/\\]\\S-+\\)" nil t)
          (let ((local-path (expand-file-name (match-string 1) go-mod)))
            (when (file-directory-p local-path)
              (push local-path replacements))))))
    replacements))

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

;; Debug function for troubleshooting
(defun debug-go-flycheck ()
  "Debug function to check Go flycheck setup."
  (interactive)
  (message "=== Go Flycheck Debug ===")
  (message "Flycheck loaded: %s" (featurep 'flycheck))
  (message "Current major mode: %s" major-mode)
  (when (eq major-mode 'go-mode)
    (message "Current checker: %s" flycheck-checker)
    (message "Disabled checkers: %s" flycheck-disabled-checkers)
    (message "--- Individual Go Checkers Status ---")
    (dolist (checker '(go-gofmt go-vet go-build go-test go-errcheck go-unconvert go-staticcheck))
      (message "%s: valid=%s, executable=%s, disabled=%s"
               checker
               (flycheck-valid-checker-p checker)
               (when (flycheck-checker-executable checker)
                 (executable-find (flycheck-checker-executable checker)))
               (member checker flycheck-disabled-checkers)))
    (message "--- Checker Chain ---")
    (let ((checker 'go-gofmt))
      (while checker
        (message "%s -> %s" checker (flycheck-checker-get checker 'next-checkers))
        (setq checker (car (flycheck-checker-get checker 'next-checkers))))))
  (message "========================"))

;; Function to manually run a specific checker
(defun run-go-checker (checker)
  "Manually run a specific Go checker on the current buffer."
  (interactive
   (list (intern (completing-read "Checker: "
                                  '("go-gofmt" "go-vet" "go-build" "go-errcheck"
                                    "go-unconvert" "go-staticcheck")))))
  (if (flycheck-valid-checker-p checker)
      (flycheck-compile checker)
    (message "Checker %s is not valid" checker)))

;; Function to reload Go flycheck configuration
(defun reload-go-flycheck-config ()
  "Reload the Go flycheck configuration."
  (interactive)
  (when (eq major-mode 'go-mode)
    ;; Re-run the configuration
    (configure-go-checker-chain)
    ;; Force buffer to be rechecked
    (flycheck-buffer)
    (message "Go flycheck configuration reloaded")))

(defun test-go-flycheck ()
  "Test individual Go linters with a sample Go file."
  (interactive)
  (let ((test-file "/tmp/test-go-linters.go"))
    (with-temp-file test-file
      (insert "package main\n\n")
      (insert "import \"fmt\"\n")
      (insert "import \"os\"\n\n")
      (insert "// main is the entry point\n")
      (insert "func main() {\n")
      (insert "\t// Formatting issue: should use gofmt\n")
      (insert "x:=5\n")
      (insert "\t// Vet issue: incorrect format string\n")
      (insert "\tfmt.Printf(\"%s\", 123)\n")
      (insert "\t// Errcheck issue: unchecked error\n")
      (insert "\tos.Open(\"test.txt\")\n")
      (insert "\t// Unconvert issue: unnecessary type conversion\n")
      (insert "\ty := int(x)\n")
      (insert "\t// Staticcheck issue: deprecated function (example)\n")
      (insert "\tfmt.Println(y)\n")
      (insert "}\n"))
    (find-file test-file)
    (go-mode)
    (message "Test file created with various linter issues. Save the file to see errors.")))

;;; init-golang.el ends here
