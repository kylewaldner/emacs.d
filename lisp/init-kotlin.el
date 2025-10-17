;;; init-kotlin.el --- Kotlin development support -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Kotlin development with Gradle/Maven, Kotlin Language Server (LSP),
;; optional tree-sitter kotlin-ts-mode, and modern tooling.
;; Mirrors structure and conventions from init-scala.el and uses unified LSP settings.
;;
;; AUTO IMPORT FUNCTIONALITY:
;; - Auto imports are enabled through LSP code actions and completion
;; - Use C-c I to trigger auto import at point
;; - Use C-c C-o to organize imports and format buffer
;; - Use C-c i for standard lsp-organize-imports
;; - Completion will automatically suggest and add imports when accepting items
;; - Code actions (including imports) are shown in sideline and via C-c l a a
;;; Code:

;; Load unified LSP configuration first
(require 'init-lsp-unified)

;; Silence byte-compile warnings when apheleia isn't loaded yet
(eval-when-compile
  (defvar apheleia-formatters nil)
  (defvar apheleia-mode-alist nil))

;; Core Kotlin support
(straight-use-package 'kotlin-mode)

;; Modern tree-sitter support for Kotlin (Emacs 29+)
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  ;; Install kotlin-ts-mode for modern tree-sitter support if available
  (straight-use-package 'kotlin-ts-mode)

  ;; Set up the tree-sitter grammar if needed
  (when (featurep 'kotlin-ts-mode)
    ;; Initialize treesit-language-source-alist if it doesn't exist
    (unless (boundp 'treesit-language-source-alist)
      (setq treesit-language-source-alist nil))

    (add-to-list 'treesit-language-source-alist
                 '(kotlin "https://github.com/fwcd/tree-sitter-kotlin" "master" "src"))

    ;; Auto-install grammar if not available
    (unless (treesit-language-available-p 'kotlin)
      (message "Kotlin tree-sitter grammar not found, attempting to install...")
      (condition-case err
          (treesit-install-language-grammar 'kotlin)
        (error (message "Failed to auto-install Kotlin tree-sitter grammar: %s" err))))

    ;; Always set up auto-mode-alist, even if grammar install failed
    (message "Setting up kotlin-ts-mode as default for .kt/.kts files...")

    ;; Remove any existing entries and add tree-sitter mode first
    (setq auto-mode-alist
          (cons '("\\.kt\\'" . kotlin-ts-mode)
                (assq-delete-all "\\.kt\\'" auto-mode-alist)))
    (setq auto-mode-alist
          (cons '("\\.kts\\'" . kotlin-ts-mode)
                (assq-delete-all "\\.kts\\'" auto-mode-alist)))

    ;; Set up major mode remap
    (add-to-list 'major-mode-remap-alist '(kotlin-mode . kotlin-ts-mode))

    (message "Kotlin tree-sitter mode configured as default")))

;; Fallback to regular kotlin-mode for older Emacs versions or missing grammar
(unless (and (fboundp 'treesit-available-p) (treesit-available-p))
  (add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode))
  (add-to-list 'auto-mode-alist '("\\.kts\\'" . kotlin-mode))
  (message "Using regular kotlin-mode (tree-sitter not available)"))

;; Kotlin-specific LSP configuration
(with-eval-after-load 'lsp-mode
  ;; Prevent any auto-download prompts. We'll install servers manually.
  (setq lsp-enable-suggest-server-download nil)
  (when (boundp 'lsp-auto-install-server)
    (setq lsp-auto-install-server nil))

  ;; Ensure Kotlin is registered for kotlin files
  (add-to-list 'lsp-language-id-configuration '(kotlin-mode . "kotlin"))
  (when (fboundp 'kotlin-ts-mode)
    (add-to-list 'lsp-language-id-configuration '(kotlin-ts-mode . "kotlin")))

  ;; Explicitly disable any built-in Kotlin client that might try to download
  (when (boundp 'lsp-disabled-clients)
    (add-to-list 'lsp-disabled-clients 'kotlin-ls)
    (add-to-list 'lsp-disabled-clients 'kotlin-language-server))

  ;; Register Kotlin Language Server
  (when (executable-find "kotlin-language-server")
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("kotlin-language-server"))
                      :major-modes '(kotlin-mode kotlin-ts-mode)
                      :priority 10
                      :server-id 'kotlinls)))

  ;; Hook LSP to Kotlin modes
  ;;(add-hook 'kotlin-mode-hook (lambda () (unless (file-remote-p default-directory) (lsp-deferred))))
  (when (fboundp 'kotlin-ts-mode)
    (add-hook 'kotlin-ts-mode-hook (lambda () (unless (file-remote-p default-directory) (lsp-deferred))))))

;; Tree-sitter structural editing (Emacs 29+)
(when (and (straight-use-package 'combobulate)
           (fboundp 'treesit-available-p)
           (treesit-available-p))
  (when (fboundp 'kotlin-ts-mode)
    (add-hook 'kotlin-ts-mode-hook 'combobulate-mode)))

;; Modern code formatting
(when (straight-use-package 'apheleia)
  ;; Prefer ktfmt if available (supports stdin)
  (when (executable-find "ktfmt")
    (after-load 'apheleia
      (add-to-list 'apheleia-formatters
                   '(ktfmt "ktfmt" "-"))
      (add-to-list 'apheleia-mode-alist
                   '(kotlin-mode . ktfmt))
      (when (fboundp 'kotlin-ts-mode)
        (add-to-list 'apheleia-mode-alist
                     '(kotlin-ts-mode . ktfmt))))))

;; Project management integration
(when (straight-use-package 'projectile)
  (after-load 'projectile
    ;; Recognize Gradle Kotlin DSL projects and Maven
    (projectile-register-project-type 'gradle-kts '("build.gradle.kts")
                                      :project-file "build.gradle.kts"
                                      :compile "./gradlew build"
                                      :test "./gradlew test"
                                      :run "./gradlew run")
    (projectile-register-project-type 'maven '("pom.xml")
                                      :project-file "pom.xml"
                                      :compile "mvn -q -DskipTests package"
                                      :test "mvn -q test"
                                      :run "mvn -q exec:java")))

;; Snippet support
(when (straight-use-package 'yasnippet)
  (add-hook 'kotlin-mode-hook 'yas-minor-mode)
  (when (fboundp 'kotlin-ts-mode)
    (add-hook 'kotlin-ts-mode-hook 'yas-minor-mode)))

;; Useful keybindings for Kotlin development (in addition to standard LSP bindings)
(defun kotlin-mode-setup-keybindings ()
  "Set up useful keybindings for Kotlin development."
  ;; Additional Kotlin-specific LSP bindings
  (when (featurep 'lsp-mode)
    (local-set-key (kbd "C-c i") 'lsp-organize-imports)
    (local-set-key (kbd "C-c r") 'lsp-rename)
    (local-set-key (kbd "C-c f") 'lsp-format-buffer)
    ;; Auto import keybindings (added by kotlin-quick-import-setup)
    ;; C-c I   - Auto import symbol at point
    ;; C-c C-o - Organize imports and format buffer
    ))

(add-hook 'kotlin-mode-hook 'kotlin-mode-setup-keybindings)
(when (fboundp 'kotlin-ts-mode)
  (add-hook 'kotlin-ts-mode-hook 'kotlin-mode-setup-keybindings))

;; Enhanced auto import functionality
(defun kotlin-auto-import-at-point ()
  "Attempt to auto import the symbol at point using LSP code actions."
  (interactive)
  (if (and (featurep 'lsp-mode) (lsp-workspaces))
      (let ((actions (lsp-code-actions-at-point)))
        (if actions
            (progn
              ;; Look for import-related actions
              (let ((import-actions (seq-filter
                                     (lambda (action)
                                       (let ((title (gethash "title" action "")))
                                         (or (string-match-p "import" title)
                                             (string-match-p "Import" title))))
                                     actions)))
                (if import-actions
                    (lsp-execute-code-action (car import-actions))
                  ;; Fallback: show all code actions if no import actions found
                  (call-interactively 'lsp-execute-code-action))))
          (message "No code actions available at point"))
        (message "LSP not active or no workspace available"))))

(defun kotlin-organize-imports-and-format ()
  "Organize imports and format the buffer."
  (interactive)
  (when (and (featurep 'lsp-mode) (lsp-workspaces))
    (lsp-organize-imports)
    (lsp-format-buffer)))

;; Bind helpers in Kotlin modes
(defun kotlin-quick-import-setup ()
  (local-set-key (kbd "C-c I") 'kotlin-auto-import-at-point)
  (local-set-key (kbd "C-c C-o") 'kotlin-organize-imports-and-format))

(add-hook 'kotlin-mode-hook 'kotlin-quick-import-setup)
(when (fboundp 'kotlin-ts-mode)
  (add-hook 'kotlin-ts-mode-hook 'kotlin-quick-import-setup))

;; Helper function to install tree-sitter grammar
(defun install-kotlin-treesitter-grammar ()
  "Install the Kotlin tree-sitter grammar if not already available."
  (interactive)
  (if (and (fboundp 'treesit-available-p) (treesit-available-p))
      (if (and (fboundp 'treesit-language-available-p)
               (treesit-language-available-p 'kotlin))
          (message "Kotlin tree-sitter grammar is already installed and available")
        (progn
          (message "Installing Kotlin tree-sitter grammar...")
          ;; Ensure the language source is properly configured
          (unless (boundp 'treesit-language-source-alist)
            (setq treesit-language-source-alist nil))

          ;; Remove any existing entry and add the correct one
          (setq treesit-language-source-alist
                (assq-delete-all 'kotlin treesit-language-source-alist))
          (add-to-list 'treesit-language-source-alist
                       '(kotlin "https://github.com/fwcd/tree-sitter-kotlin" "master" "src"))

          (condition-case err
              (progn
                (treesit-install-language-grammar 'kotlin)
                (if (and (fboundp 'treesit-language-available-p)
                         (treesit-language-available-p 'kotlin))
                    (message "Kotlin tree-sitter grammar installed successfully!")
                  (message "⚠ Grammar installation completed but language not available")))
            (error (message "✗ Failed to install Kotlin tree-sitter grammar: %s" err)))))
    (message "Tree-sitter is not available in this Emacs build")))

;; Helper function to force reinstall tree-sitter grammar
(defun reinstall-kotlin-treesitter-grammar ()
  "Force reinstall the Kotlin tree-sitter grammar, even if already installed."
  (interactive)
  (if (and (fboundp 'treesit-available-p) (treesit-available-p))
      (progn
        (message "Force reinstalling Kotlin tree-sitter grammar...")
        ;; Ensure the language source is properly configured
        (unless (boundp 'treesit-language-source-alist)
          (setq treesit-language-source-alist nil))

        ;; Remove any existing entry and add the correct one
        (setq treesit-language-source-alist
              (assq-delete-all 'kotlin treesit-language-source-alist))
        (add-to-list 'treesit-language-source-alist
                     '(kotlin "https://github.com/fwcd/tree-sitter-kotlin" "master" "src"))

        (condition-case err
            (progn
              ;; Force installation regardless of current status
              (treesit-install-language-grammar 'kotlin t)
              (if (and (fboundp 'treesit-language-available-p)
                       (treesit-language-available-p 'kotlin))
                  (message "Kotlin tree-sitter grammar reinstalled successfully!")
                (message "⚠ Grammar reinstallation completed but language not available")))
          (error (message "✗ Failed to reinstall Kotlin tree-sitter grammar: %s" err))))
    (message "Tree-sitter is not available in this Emacs build")))

;; Helper function to check tree-sitter grammar status
(defun kotlin-treesitter-status ()
  "Check the status of Kotlin tree-sitter support."
  (interactive)
  (if (and (fboundp 'treesit-available-p) (treesit-available-p))
      (if (and (fboundp 'treesit-language-available-p)
               (treesit-language-available-p 'kotlin))
          (message "Kotlin tree-sitter grammar is available")
        (message "⚠ Kotlin tree-sitter grammar is not available. Run M-x install-kotlin-treesitter-grammar"))
    (message "Tree-sitter is not available in this Emacs build")))

;; Helper function to ensure Kotlin LSP is active
(defun kotlin-ensure-kotlinls-lsp ()
  "Ensure Kotlin Language Server is the active LSP server for the current Kotlin buffer."
  (interactive)
  (when (and (or (eq major-mode 'kotlin-mode) (eq major-mode 'kotlin-ts-mode))
             (featurep 'lsp-mode))
    (let ((workspace (car (lsp-workspaces))))
      (if workspace
          (let ((server-id (lsp--workspace-server-id workspace)))
            (if (eq server-id 'kotlinls)
                (message "Kotlin LSP server is active")
              (progn
                (message "Different LSP server active: %s. Restarting..." server-id)
                (lsp-workspace-shutdown)
                (lsp-workspace-restart))))
        (progn
          (message "Starting Kotlin LSP server...")
          (lsp))))))

(provide 'init-kotlin)
;;; init-kotlin.el ends here
