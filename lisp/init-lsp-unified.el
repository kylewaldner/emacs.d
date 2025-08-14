;;; init-lsp-unified.el --- Unified LSP configuration with standardized keybindings -*- lexical-binding: t -*-
;;; Commentary:
;; Unified LSP setup that ensures consistent keybindings across all programming languages
;; Uses standard C-c l prefix and explicitly disables conflicting Super key bindings
;;; Code:

;; Set file watch ignore patterns BEFORE loading lsp-mode
;; This ensures they're applied when LSP servers start
(setq lsp-file-watch-ignored-directories
      '(;; Version control
        "[/\\\\]\\.git\\'"
        "[/\\\\]\\.github\\'"
        "[/\\\\]\\.circleci\\'"
        ;; Go specific
        "[/\\\\]vendor\\'"
        "[/\\\\]\\.vendor\\'"
        "[/\\\\]bin\\'"
        "[/\\\\]build\\'"
        "[/\\\\]dist\\'"
        "[/\\\\]target\\'"
        ;; Node/JS (in case of mixed projects)
        "[/\\\\]node_modules\\'"
        ;; Python
        "[/\\\\]\\.venv\\'"
        "[/\\\\]venv\\'"
        "[/\\\\]__pycache__\\'"
        "[/\\\\]\\.pytest_cache\\'"
        "[/\\\\]\\.mypy_cache\\'"
        ;; Build artifacts
        "[/\\\\]\\.cache\\'"
        "[/\\\\]\\.tmp\\'"
        "[/\\\\]tmp\\'"
        ;; IDE
        "[/\\\\]\\.idea\\'"
        "[/\\\\]\\.vscode\\'"
        ;; OS
        "[/\\\\]\\.DS_Store\\'"
        ;; Archives
        "[/\\\\].*\\.zip\\'"
        "[/\\\\].*\\.tar\\.gz\\'"
        ;; Go test cache
        "[/\\\\]\\.test\\'"
        "[/\\\\]testdata\\'"
        ;; Coverage
        "[/\\\\]coverage\\'"
        "[/\\\\]htmlcov\\'"
        "[/\\\\]\\.coverage\\'"))

;; Additional file patterns to ignore
(setq lsp-file-watch-ignored-files
      '(;; Lock files
        "[/\\\\]\\.#.*\\'"
        ;; Backup files
        "[/\\\\].*~\\'"
        ;; Binary files
        "[/\\\\].*\\.exe\\'"
        "[/\\\\].*\\.dll\\'"
        "[/\\\\].*\\.so\\'"
        "[/\\\\].*\\.dylib\\'"
        ;; Go binaries (often no extension)
        "[/\\\\][^/\\\\]*[^/\\\\.]+$"  ; Files without extension in root
        ;; Archives
        "[/\\\\].*\\.zip\\'"
        "[/\\\\].*\\.tar\\'"
        "[/\\\\].*\\.gz\\'"
        ;; Images
        "[/\\\\].*\\.png\\'"
        "[/\\\\].*\\.jpg\\'"
        "[/\\\\].*\\.jpeg\\'"
        "[/\\\\].*\\.gif\\'"
        ;; Databases
        "[/\\\\].*\\.db\\'"
        "[/\\\\].*\\.sqlite\\'"))

;; Core LSP Mode Configuration
(when (straight-use-package 'lsp-mode)
  ;; Set standard keymap prefix - this is the ONLY prefix we want to use
  (setq lsp-keymap-prefix "C-c l")

  (setq lsp-enable-suggest-server-download nil)  ; Prevent automatic server downloads

  ;; Performance optimizations that apply to all languages
  (setq lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-completion-provider :company-capf
        lsp-headerline-breadcrumb-enable t
        lsp-enable-file-watchers t   ; Enable with ignore patterns
        lsp-file-watch-threshold 1000 ; Lower threshold now that we have good ignores
        lsp-enable-text-document-color nil
        lsp-signature-auto-activate nil  ; Reduce noise
        lsp-signature-render-documentation nil)

  ;; Auto import and code action settings for better IDE experience
  (setq lsp-completion-enable-additional-text-edit t  ; Automatically add imports on completion
        lsp-enable-snippet t                          ; Enable snippets
        lsp-completion-show-detail t                  ; Show detailed completion info
        lsp-completion-show-kind t                    ; Show completion item kinds
        lsp-enable-on-type-formatting nil             ; Can be enabled per language if needed
        lsp-enable-imenu t                           ; Enable imenu integration
        lsp-enable-which-key-integration t)          ; Enable which-key integration

  ;; Disable any default Super key bindings
  (with-eval-after-load 'lsp-mode
    ;; Remove any global Super key bindings that LSP might set
    (when (boundp 'lsp-command-map)
      ;; Ensure LSP command map only uses our prefix
      (define-key global-map (kbd "s-l") nil)  ; Remove Super+l
      (define-key global-map (kbd "C-c l") lsp-command-map))

    ;; Explicitly set up which-key integration with our prefix only
    (when (featurep 'which-key)
      (which-key-add-key-based-replacements
        "C-c l" "lsp"
        "C-c l g" "goto"
        "C-c l h" "help"
        "C-c l r" "refactor"
        "C-c l F" "folders"
        "C-c l T" "toggle"
        "C-c l a" "actions"
        "C-c l w" "workspace"
        "C-c l f" "format")))

  ;; Enable which-key integration for LSP
  (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration))

;; Enhanced LSP UI with consistent settings
(when (straight-use-package 'lsp-ui)
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'line
        lsp-ui-sideline-delay 0.2
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t)

  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; Company completion for all LSP modes
(when (straight-use-package 'company)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.2
        company-tooltip-align-annotations t)

  ;; Add company to all programming modes that use LSP
  (add-hook 'lsp-mode-hook 'company-mode))

;; Flycheck integration
(when (straight-use-package 'flycheck)
  (add-hook 'lsp-mode-hook 'flycheck-mode)

  (with-eval-after-load 'flycheck
    ;; Let LSP handle diagnostics, disable conflicting checkers
    (setq flycheck-indication-mode 'left-fringe
          flycheck-highlighting-mode 'lines)))

;; Debug Adapter Protocol support (optional)
(when (straight-use-package 'dap-mode)
  (add-hook 'lsp-mode-hook 'dap-mode)
  (add-hook 'lsp-mode-hook 'dap-ui-mode)
  (dap-auto-configure-mode))

;; Function to set up standard LSP keybindings for any mode
(defun lsp-setup-standard-keybindings ()
  "Set up standard LSP keybindings using only C-c l prefix."
  (when (bound-and-true-p lsp-mode)
    ;; Navigation
    (local-set-key (kbd "M-.") 'lsp-find-definition)
    (local-set-key (kbd "M-?") 'lsp-find-references)
    (local-set-key (kbd "M-,") 'xref-go-back)

    ;; Documentation and help
    (local-set-key (kbd "C-c C-d") 'lsp-describe-thing-at-point)

    ;; Refactoring (keeping it simple)
    (local-set-key (kbd "C-c C-r") 'lsp-rename)

    ;; Make sure we don't have any Super key bindings
    (local-unset-key (kbd "s-l"))

    ;; Explicitly ensure our C-c l prefix works
    (local-set-key (kbd "C-c l") lsp-command-map)))

;; Add the standard keybindings to all LSP-enabled modes
(add-hook 'lsp-mode-hook 'lsp-setup-standard-keybindings)

;; Language-specific LSP server configurations
(with-eval-after-load 'lsp-mode
  ;; Python LSP setup
  (when (executable-find "pylsp")
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "pylsp")
                      :major-modes '(python-mode)
                      :server-id 'pylsp)))

  ;; JavaScript/TypeScript LSP setup
  (when (executable-find "typescript-language-server")
    (setq lsp-clients-typescript-server-args '("--stdio")))

  ;; Metals (Scala) LSP setup
  (when (executable-find "metals")
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("metals"))
                      :major-modes '(scala-mode scala-ts-mode)
                      :priority 10
                      :server-id 'metals))))

;; Function to disable Super key bindings globally (if they exist)
(defun disable-super-lsp-bindings ()
  "Ensure no Super+l bindings interfere with other applications."
  (interactive)
  ;; Remove any global Super+l bindings
  (global-unset-key (kbd "s-l"))

  ;; Also check and remove from various mode maps if they exist
  (dolist (map (list global-map))
    (when (keymapp map)
      (define-key map (kbd "s-l") nil)))

  (message "Super+l bindings disabled to prevent conflicts"))

;; Call the function to clean up any existing Super bindings
(disable-super-lsp-bindings)

;; Register the custom workspace configuration
(with-eval-after-load 'lsp-mode
  ;; Add more patterns to ignore go module cache and go installation
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]go[/\\\\]pkg[/\\\\]mod\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]usr[/\\\\]local[/\\\\]go\\'"))

;; Debug function to check what directories LSP wants to watch
(defun lsp-debug-file-watchers ()
  "Show information about LSP file watching in the current buffer."
  (interactive)
  (if lsp-mode
      (let* ((workspace (lsp-workspaces))
             (root (when workspace (lsp-workspace-root (car workspace)))))
        (with-current-buffer (get-buffer-create "*LSP File Watch Debug*")
          (erase-buffer)
          (insert "LSP File Watching Debug Information\n")
          (insert "===================================\n\n")
          (insert (format "Workspace root: %s\n" (or root "Not found")))
          (insert (format "File watchers enabled: %s\n" lsp-enable-file-watchers))
          (insert (format "File watch threshold: %d\n" lsp-file-watch-threshold))
          (insert "\nIgnored directories:\n")
          (dolist (pattern lsp-file-watch-ignored-directories)
            (insert (format "  %s\n" pattern)))
          (insert "\nIgnored files:\n")
          (dolist (pattern lsp-file-watch-ignored-files)
            (insert (format "  %s\n" pattern)))
          (display-buffer (current-buffer))))
    (message "LSP mode is not active in this buffer")))

;; Display helpful information about LSP keybindings
(defun lsp-show-keybindings ()
  "Display a help buffer with all LSP keybindings."
  (interactive)
  (with-help-window "*LSP Keybindings*"
    (princ "LSP Mode Keybindings (Standard Configuration)\n")
    (princ "==============================================\n\n")
    (princ "Main Prefix: C-c l\n\n")
    (princ "Navigation:\n")
    (princ "  M-.         - Go to definition\n")
    (princ "  M-?         - Find references\n")
    (princ "  M-,         - Go back\n")
    (princ "  C-c l g d   - Go to definition (alternative)\n")
    (princ "  C-c l g r   - Find references (alternative)\n")
    (princ "  C-c l g i   - Go to implementation\n")
    (princ "  C-c l g t   - Go to type definition\n\n")
    (princ "Documentation:\n")
    (princ "  C-c C-d     - Describe thing at point\n")
    (princ "  C-c l h h   - Show hover info\n")
    (princ "  C-c l h s   - Show signature help\n\n")
    (princ "Refactoring:\n")
    (princ "  C-c C-r     - Rename symbol\n")
    (princ "  C-c l r r   - Rename (alternative)\n")
    (princ "  C-c l a a   - Execute code action\n")
    (princ "  C-c l a o   - Organize imports\n\n")
    (princ "Formatting:\n")
    (princ "  C-c l f f   - Format buffer\n")
    (princ "  C-c l f r   - Format region\n\n")
    (princ "Workspace:\n")
    (princ "  C-c l w r   - Restart LSP workspace\n")
    (princ "  C-c l w s   - LSP shutdown\n")
    (princ "  C-c l w d   - Describe session\n\n")
    (princ "Note: All Super key (s-l) bindings have been disabled\n")
    (princ "to prevent conflicts with other applications.\n")))

;; Add to LSP mode hook to show help the first time
(defvar lsp-first-time-setup t
  "Flag to show help message on first LSP activation.")

(defun lsp-show-first-time-help ()
  "Show help message on first LSP activation."
  (when lsp-first-time-setup
    (setq lsp-first-time-setup nil)
    (run-with-timer 2 nil
                    (lambda ()
                      (message "LSP activated! Use C-c l for LSP commands. Run M-x lsp-show-keybindings for full help.")))))

(add-hook 'lsp-mode-hook 'lsp-show-first-time-help)

(provide 'init-lsp-unified)
;;; init-lsp-unified.el ends here
