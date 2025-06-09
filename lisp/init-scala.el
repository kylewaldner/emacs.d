;;; init-scala.el --- Support for scala and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Modern Scala development environment with tree-sitter, lsp-mode, and productivity enhancements
;;; Code:

;; Core Scala support
(straight-use-package 'scala-mode)

;; Modern tree-sitter support for Scala (Emacs 29+)
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  ;; Install scala-ts-mode for modern tree-sitter support
  (straight-use-package 'scala-ts-mode)

  ;; Set up the tree-sitter grammar if needed
  (when (straight-use-package 'scala-ts-mode)
    ;; Initialize treesit-language-source-alist if it doesn't exist
    (unless (boundp 'treesit-language-source-alist)
      (setq treesit-language-source-alist nil))

    (add-to-list 'treesit-language-source-alist
                 '(scala "https://github.com/tree-sitter/tree-sitter-scala" "master" "src"))

    ;; Auto-install grammar if not available
    (unless (treesit-language-available-p 'scala)
      (message "Scala tree-sitter grammar not found, attempting to install...")
      (condition-case err
          (treesit-install-language-grammar 'scala)
        (error (message "Failed to auto-install Scala tree-sitter grammar: %s" err))))

    ;; Only remap if grammar is available
    (when (treesit-language-available-p 'scala)
      ;; Remap scala-mode to scala-ts-mode for better performance
      (add-to-list 'major-mode-remap-alist '(scala-mode . scala-ts-mode))

      ;; Set up auto-mode-alist for Scala files
      (add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-ts-mode)))))

;; Fallback to regular scala-mode for older Emacs versions
(unless (and (fboundp 'treesit-available-p) (treesit-available-p))
  (add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
  (add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-mode)))

;; SBT integration with modern settings
(when (straight-use-package 'sbt-mode)
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)

  ;; sbt-supershell kills sbt-mode: https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))

  ;; Better sbt buffer management
  (setq sbt:prefer-nested-projects t)
  (setq sbt:scroll-to-bottom-on-output nil))

;; Modern LSP setup with optimized performance
(when (straight-use-package 'lsp-mode)
  ;; Performance optimizations
  (setq lsp-prefer-flymake nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-idle-delay 0.500)
  (setq lsp-completion-provider :capf)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-text-document-color nil)

  ;; Scala-specific LSP settings
  (setq lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))

  ;; Hook into Scala modes
  (add-hook 'scala-mode-hook 'lsp-deferred)
  (when (featurep 'scala-ts-mode)
    (add-hook 'scala-ts-mode-hook 'lsp-deferred))

  ;; Enable helpful features
  (add-hook 'lsp-mode-hook 'lsp-lens-mode)
  (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration))

;; Metals language server configuration
(when (straight-use-package 'lsp-metals)
  (setq lsp-metals-treeview-show-when-views-received t)
  (setq lsp-metals-enable-semantic-highlighting t)

  ;; Enable Metals specific features
  (setq lsp-metals-show-implicit-arguments t)
  (setq lsp-metals-show-implicit-conversions-and-classes t)
  (setq lsp-metals-show-inferred-type t))

;; Enhanced LSP UI
(when (straight-use-package 'lsp-ui)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-show-directory t))

;; Modern completion with Corfu (if available)
(when (straight-use-package 'corfu)
  (add-hook 'scala-mode-hook (lambda ()
                               (setq-local corfu-auto t)
                               (setq-local corfu-auto-delay 0.1)
                               (setq-local corfu-auto-prefix 2)
                               (corfu-mode))))

;; Alternative: Company mode for completion
(when (and (straight-use-package 'company) (not (featurep 'corfu)))
  (add-hook 'scala-mode-hook 'company-mode)
  (when (featurep 'scala-ts-mode)
    (add-hook 'scala-ts-mode-hook 'company-mode)))

;; Debug Adapter Protocol support
(when (straight-use-package 'dap-mode)
  (add-hook 'lsp-mode-hook 'dap-mode)
  (add-hook 'lsp-mode-hook 'dap-ui-mode)

  ;; Scala debugging setup
  (require 'dap-mode)
  (when (featurep 'dap-mode)
    (dap-auto-configure-mode)))

;; Tree-sitter structural editing (Emacs 29+)
(when (and (straight-use-package 'combobulate)
           (fboundp 'treesit-available-p)
           (treesit-available-p))
  (add-hook 'scala-ts-mode-hook 'combobulate-mode))

;; Modern code formatting
(when (straight-use-package 'apheleia)
  ;; Scalafmt integration
  (when (executable-find "scalafmt")
    (after-load 'apheleia
      (add-to-list 'apheleia-formatters
                   '(scalafmt "scalafmt" "--stdin"))
      (add-to-list 'apheleia-mode-alist
                   '(scala-mode . scalafmt))
      (when (featurep 'scala-ts-mode)
        (add-to-list 'apheleia-mode-alist
                     '(scala-ts-mode . scalafmt))))))

;; Enhanced error checking
(when (straight-use-package 'flycheck)
  (add-hook 'scala-mode-hook 'flycheck-mode)
  (when (featurep 'scala-ts-mode)
    (add-hook 'scala-ts-mode-hook 'flycheck-mode)))

;; Project management integration
(when (straight-use-package 'projectile)
  (after-load 'projectile
    (projectile-register-project-type 'sbt '("build.sbt")
                                      :project-file "build.sbt"
                                      :compile "sbt compile"
                                      :test "sbt test"
                                      :run "sbt run"
                                      :test-suffix "Spec")))

;; Snippet support
(when (straight-use-package 'yasnippet)
  (add-hook 'scala-mode-hook 'yas-minor-mode)
  (when (featurep 'scala-ts-mode)
    (add-hook 'scala-ts-mode-hook 'yas-minor-mode)))

;; Which-key integration for LSP commands
(when (straight-use-package 'which-key)
  (add-hook 'scala-mode-hook
            (lambda ()
              (which-key-add-major-mode-key-based-replacements 'scala-mode
                "C-c l" "lsp"
                "C-c l g" "goto"
                "C-c l h" "help"
                "C-c l r" "refactor"
                "C-c l F" "folders"
                "C-c l T" "toggle"
                "C-c l a" "actions"
                "C-c l w" "workspace")))

  (when (featurep 'scala-ts-mode)
    (add-hook 'scala-ts-mode-hook
              (lambda ()
                (which-key-add-major-mode-key-based-replacements 'scala-ts-mode
                  "C-c l" "lsp"
                  "C-c l g" "goto"
                  "C-c l h" "help"
                  "C-c l r" "refactor"
                  "C-c l F" "folders"
                  "C-c l T" "toggle"
                  "C-c l a" "actions"
                  "C-c l w" "workspace")))))

;; Useful keybindings for Scala development
(defun scala-mode-setup-keybindings ()
  "Set up useful keybindings for Scala development."
  (local-set-key (kbd "C-c C-b") 'sbt-command)
  (local-set-key (kbd "C-c C-c") 'sbt-do-compile)
  (local-set-key (kbd "C-c C-t") 'sbt-do-test)
  (local-set-key (kbd "C-c C-r") 'sbt-do-run)
  (when (featurep 'lsp-mode)
    (local-set-key (kbd "C-c i") 'lsp-organize-imports)
    (local-set-key (kbd "C-c r") 'lsp-rename)
    (local-set-key (kbd "C-c f") 'lsp-format-buffer)))

(add-hook 'scala-mode-hook 'scala-mode-setup-keybindings)
(when (featurep 'scala-ts-mode)
  (add-hook 'scala-ts-mode-hook 'scala-mode-setup-keybindings))

;; Performance tip for large Scala projects
(defun scala-performance-settings ()
  "Apply performance settings for large Scala projects."
  (setq-local lsp-file-watch-threshold 5000)
  (setq-local lsp-enable-file-watchers nil)
  (setq-local lsp-enable-semantic-highlighting nil))

(add-hook 'scala-mode-hook 'scala-performance-settings)
(when (featurep 'scala-ts-mode)
  (add-hook 'scala-ts-mode-hook 'scala-performance-settings))

;; Helper function to install tree-sitter grammar
(defun install-scala-treesitter-grammar ()
  "Install the Scala tree-sitter grammar if not already available."
  (interactive)
  (if (and (fboundp 'treesit-available-p) (treesit-available-p))
      (if (and (fboundp 'treesit-language-available-p)
               (treesit-language-available-p 'scala))
          (message "Scala tree-sitter grammar is already installed and available")
        (progn
          (message "Installing Scala tree-sitter grammar...")
          ;; Ensure the language source is properly configured
          (unless (boundp 'treesit-language-source-alist)
            (setq treesit-language-source-alist nil))

          ;; Remove any existing entry and add the correct one
          (setq treesit-language-source-alist
                (assq-delete-all 'scala treesit-language-source-alist))
          (add-to-list 'treesit-language-source-alist
                       '(scala "https://github.com/tree-sitter/tree-sitter-scala" "master" "src"))

          (condition-case err
              (progn
                (treesit-install-language-grammar 'scala)
                (if (and (fboundp 'treesit-language-available-p)
                         (treesit-language-available-p 'scala))
                    (message "✓ Scala tree-sitter grammar installed successfully!")
                  (message "⚠ Grammar installation completed but language not available")))
            (error (message "✗ Failed to install Scala tree-sitter grammar: %s" err)))))
    (message "Tree-sitter is not available in this Emacs build")))

;; Helper function to force reinstall tree-sitter grammar
(defun reinstall-scala-treesitter-grammar ()
  "Force reinstall the Scala tree-sitter grammar, even if already installed."
  (interactive)
  (if (and (fboundp 'treesit-available-p) (treesit-available-p))
      (progn
        (message "Force reinstalling Scala tree-sitter grammar...")
        ;; Ensure the language source is properly configured
        (unless (boundp 'treesit-language-source-alist)
          (setq treesit-language-source-alist nil))

        ;; Remove any existing entry and add the correct one
        (setq treesit-language-source-alist
              (assq-delete-all 'scala treesit-language-source-alist))
        (add-to-list 'treesit-language-source-alist
                     '(scala "https://github.com/tree-sitter/tree-sitter-scala" "master" "src"))

        (condition-case err
            (progn
              ;; Force installation regardless of current status
              (treesit-install-language-grammar 'scala t) ; The 't' forces reinstallation
              (if (and (fboundp 'treesit-language-available-p)
                       (treesit-language-available-p 'scala))
                  (message "✓ Scala tree-sitter grammar reinstalled successfully!")
                (message "⚠ Grammar reinstallation completed but language not available")))
          (error (message "✗ Failed to reinstall Scala tree-sitter grammar: %s" err))))
    (message "Tree-sitter is not available in this Emacs build")))

;; Helper function to check tree-sitter grammar status
(defun scala-treesitter-status ()
  "Check the status of Scala tree-sitter support."
  (interactive)
  (if (and (fboundp 'treesit-available-p) (treesit-available-p))
      (if (and (fboundp 'treesit-language-available-p)
               (treesit-language-available-p 'scala))
          (message "✓ Scala tree-sitter grammar is available")
        (message "⚠ Scala tree-sitter grammar is not available. Run M-x install-scala-treesitter-grammar"))
    (message "Tree-sitter is not available in this Emacs build")))

(provide 'init-scala)
;;; init-scala.el ends here
