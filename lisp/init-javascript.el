;;; init-javascript.el --- Modern JavaScript/TypeScript development -*- lexical-binding: t -*-
;;; Commentary:
;; Modern JavaScript development setup with LSP, tree-sitter, and best practices
;;; Code:

;;; Core Requirements
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'company)
(straight-use-package 'flycheck)
(straight-use-package 'projectile)

;;; JavaScript/TypeScript Modes

;; Use tree-sitter modes for Emacs 29+ (modern syntax highlighting and parsing)
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  ;; Install tree-sitter grammars
  ;; Initialize treesit-language-source-alist if it doesn't exist
  (unless (boundp 'treesit-language-source-alist)
    (setq treesit-language-source-alist nil))

  (setq treesit-language-source-alist
        (append treesit-language-source-alist
                '((javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                  (json "https://github.com/tree-sitter/tree-sitter-json" "master" "src"))))

  ;; Auto-install grammars if not available
  (dolist (lang '(javascript typescript tsx json))
    (unless (treesit-language-available-p lang)
      (message "Installing %s tree-sitter grammar..." lang)
      (condition-case err
          (treesit-install-language-grammar lang)
        (error (message "Failed to install %s grammar: %s" lang err)))))

  ;; Remap to tree-sitter modes when available
  (when (treesit-language-available-p 'javascript)
    (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
    (add-to-list 'major-mode-remap-alist '(js2-mode . js-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.\\(js\\|mjs\\|cjs\\)\\'" . js-ts-mode)))

  (when (treesit-language-available-p 'typescript)
    (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))

  (when (treesit-language-available-p 'tsx)
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

  (when (treesit-language-available-p 'json)
    (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))))

;; Fallback to traditional modes for older Emacs or missing grammars
(unless (and (fboundp 'treesit-available-p) (treesit-available-p))
  ;; Install traditional JS/TS packages
  (straight-use-package 'js2-mode)
  (straight-use-package 'typescript-mode)
  (straight-use-package 'rjsx-mode)
  (straight-use-package 'json-mode)

  ;; File associations
  (add-to-list 'auto-mode-alist '("\\.\\(js\\|mjs\\|cjs\\)\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

  ;; js2-mode configuration
  (after-load 'js2-mode
    (setq-default js2-bounce-indent-p nil
                  js2-mode-show-parse-errors nil
                  js2-mode-show-strict-warnings nil
                  js2-basic-offset 2)
    (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))))

;;; Web Development Support
(straight-use-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))

(after-load 'web-mode
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-current-element-highlight t))

;;; Language Server Protocol (LSP) Setup

(when (straight-use-package 'lsp-mode)
  ;; Performance optimizations
  (setq lsp-keymap-prefix "C-c l"
        lsp-idle-delay 0.5
        lsp-completion-provider :company-capf
        lsp-headerline-breadcrumb-enable t
        lsp-enable-file-watchers nil  ; Disabled for performance
        lsp-file-watch-threshold 5000
        lsp-log-io nil
        lsp-enable-text-document-color nil)

  ;; JavaScript/TypeScript LSP servers
  ;; Priority: typescript-language-server > vscode-langservers-extracted
  (when (executable-find "typescript-language-server")
    (setq lsp-clients-typescript-server-args '("--stdio")))

  ;; Enable LSP for JS/TS modes
  (dolist (mode '(js-mode js2-mode js-ts-mode
                          typescript-mode typescript-ts-mode
                          tsx-ts-mode rjsx-mode web-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda ()
                (when (and (buffer-file-name)
                           (not (file-remote-p default-directory)))
                  (lsp-deferred)))))

  ;; JSON support
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)
              (when (not (file-remote-p default-directory))
                (lsp-deferred))))

  (when (and (fboundp 'treesit-language-available-p)
             (treesit-language-available-p 'json))
    (add-hook 'json-ts-mode-hook
              (lambda ()
                (when (not (file-remote-p default-directory))
                  (lsp-deferred))))))

;;; LSP UI Enhancements
(when (straight-use-package 'lsp-ui)
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.5
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t)

  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;;; Completion with Company
(when (straight-use-package 'company)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.2
        company-tooltip-align-annotations t)

  ;; Enable company for all JS/TS modes
  (dolist (mode '(js-mode js2-mode js-ts-mode
                  typescript-mode typescript-ts-mode
                  tsx-ts-mode rjsx-mode web-mode
                  json-mode json-ts-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook")) 'company-mode)))

;;; Syntax Checking with Flycheck
(when (straight-use-package 'flycheck)
  ;; Enable flycheck for JS/TS modes
  (dolist (mode '(js-mode js2-mode js-ts-mode
                  typescript-mode typescript-ts-mode
                  tsx-ts-mode rjsx-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook")) 'flycheck-mode))

  ;; Prefer LSP diagnostics over flycheck checkers
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers
                  '(javascript-jshint javascript-jscs))))

;;; Code Formatting

;; Modern formatting with apheleia (recommended)
(when (straight-use-package 'apheleia)
  (after-load 'apheleia
    ;; Prettier for JS/TS/JSON
    (when (or (executable-find "prettier")
              (executable-find "npx"))
      (setf (alist-get 'prettier apheleia-formatters)
            '("npx" "prettier" "--stdin-filepath" filepath))

      ;; Associate formatters with modes
      (dolist (mode '(js-mode js2-mode js-ts-mode
                      typescript-mode typescript-ts-mode
                      tsx-ts-mode rjsx-mode
                      json-mode json-ts-mode
                      web-mode))
        (setf (alist-get mode apheleia-mode-alist) 'prettier))))

  ;; Enable format on save
  (add-hook 'js-mode-hook 'apheleia-mode)
  (add-hook 'js2-mode-hook 'apheleia-mode)
  (add-hook 'typescript-mode-hook 'apheleia-mode)
  (add-hook 'rjsx-mode-hook 'apheleia-mode)
  (when (and (fboundp 'treesit-language-available-p)
             (treesit-language-available-p 'javascript))
    (add-hook 'js-ts-mode-hook 'apheleia-mode))
  (when (and (fboundp 'treesit-language-available-p)
             (treesit-language-available-p 'typescript))
    (add-hook 'typescript-ts-mode-hook 'apheleia-mode)
    (add-hook 'tsx-ts-mode-hook 'apheleia-mode))
  (when (and (fboundp 'treesit-language-available-p)
             (treesit-language-available-p 'json))
    (add-hook 'json-ts-mode-hook 'apheleia-mode)))

;; Fallback: prettier-js package (legacy)
(unless (package-installed-p 'apheleia)
  (when (straight-use-package 'prettier-js)
    (add-hook 'js2-mode-hook 'prettier-js-mode)
    (add-hook 'rjsx-mode-hook 'prettier-js-mode)
    (add-hook 'typescript-mode-hook 'prettier-js-mode)))

;;; Project Management
(when (straight-use-package 'npm-mode)
  (add-hook 'js-mode-hook 'npm-mode)
  (add-hook 'js2-mode-hook 'npm-mode)
  (add-hook 'typescript-mode-hook 'npm-mode)
  (when (and (fboundp 'treesit-language-available-p)
             (treesit-language-available-p 'javascript))
    (add-hook 'js-ts-mode-hook 'npm-mode))
  (when (and (fboundp 'treesit-language-available-p)
             (treesit-language-available-p 'typescript))
    (add-hook 'typescript-ts-mode-hook 'npm-mode)))

;; Add node_modules/.bin to PATH
(when (straight-use-package 'add-node-modules-path)
  (dolist (mode '(js-mode js2-mode js-ts-mode
                  typescript-mode typescript-ts-mode
                  tsx-ts-mode rjsx-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook")) 'add-node-modules-path)))

;;; REPL and Interactive Development

;; Node.js REPL
(when (straight-use-package 'nodejs-repl)
  (setq nodejs-repl-command "node")

  (defun setup-nodejs-repl-keybindings ()
    "Set up keybindings for Node.js REPL."
    (local-set-key (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
    (local-set-key (kbd "C-c C-j") 'nodejs-repl-send-line)
    (local-set-key (kbd "C-c C-r") 'nodejs-repl-send-region)
    (local-set-key (kbd "C-c C-c") 'nodejs-repl-send-buffer)
    (local-set-key (kbd "C-c C-l") 'nodejs-repl-load-file)
    (local-set-key (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))

  (add-hook 'js-mode-hook 'setup-nodejs-repl-keybindings)
  (add-hook 'js2-mode-hook 'setup-nodejs-repl-keybindings)
  (when (and (fboundp 'treesit-language-available-p)
             (treesit-language-available-p 'javascript))
    (add-hook 'js-ts-mode-hook 'setup-nodejs-repl-keybindings)))

;; Alternative: Skewer for live browser interaction
(when (straight-use-package 'skewer-mode)
  (skewer-setup))

;;; Documentation and Code Navigation

;; JSDoc support
(when (straight-use-package 'js-doc)
  (defun setup-js-doc-keybindings ()
    "Set up keybindings for JS documentation."
    (local-set-key (kbd "C-c d") 'js-doc-insert-function-doc)
    (local-set-key (kbd "C-c @") 'js-doc-insert-tag))

  (add-hook 'js-mode-hook 'setup-js-doc-keybindings)
  (add-hook 'js2-mode-hook 'setup-js-doc-keybindings)
  (add-hook 'typescript-mode-hook 'setup-js-doc-keybindings)
  (when (and (fboundp 'treesit-language-available-p)
             (treesit-language-available-p 'javascript))
    (add-hook 'js-ts-mode-hook 'setup-js-doc-keybindings))
  (when (and (fboundp 'treesit-language-available-p)
             (treesit-language-available-p 'typescript))
    (add-hook 'typescript-ts-mode-hook 'setup-js-doc-keybindings)))

;;; Snippets
(when (straight-use-package 'yasnippet)
  (dolist (mode '(js-mode js2-mode js-ts-mode
                  typescript-mode typescript-ts-mode
                  tsx-ts-mode rjsx-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook")) 'yas-minor-mode)))

;;; Structural Editing (Tree-sitter)
(when (and (straight-use-package 'combobulate)
           (fboundp 'treesit-available-p)
           (treesit-available-p))
  (when (and (fboundp 'treesit-language-available-p)
             (treesit-language-available-p 'javascript))
    (add-hook 'js-ts-mode-hook 'combobulate-mode))
  (when (and (fboundp 'treesit-language-available-p)
             (treesit-language-available-p 'typescript))
    (add-hook 'typescript-ts-mode-hook 'combobulate-mode)
    (add-hook 'tsx-ts-mode-hook 'combobulate-mode)))

;;; Projectile Integration
(after-load 'projectile
  ;; Register project types
  (projectile-register-project-type 'npm '("package.json")
                                    :project-file "package.json"
                                    :compile "npm run build"
                                    :test "npm test"
                                    :run "npm start"
                                    :test-suffix ".test")

  (projectile-register-project-type 'yarn '("yarn.lock")
                                    :project-file "package.json"
                                    :compile "yarn build"
                                    :test "yarn test"
                                    :run "yarn start"
                                    :test-suffix ".test"))

;;; Global Settings
(setq-default js-indent-level 2
              js2-basic-offset 2
              typescript-indent-level 2)

;; Set up interpreter mode for Node.js
(add-to-list 'interpreter-mode-alist (cons "node" 'js-mode))
(when (and (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'javascript))
  (add-to-list 'interpreter-mode-alist (cons "node" 'js-ts-mode)))

;;; Helper Functions

(defun javascript-setup-lsp ()
  "Manually set up LSP for current JavaScript/TypeScript buffer."
  (interactive)
  (lsp))

(defun javascript-toggle-tree-sitter ()
  "Toggle between tree-sitter and traditional modes for JavaScript."
  (interactive)
  (cond
   ((eq major-mode 'js-mode)
    (when (and (fboundp 'treesit-language-available-p)
               (treesit-language-available-p 'javascript))
      (js-ts-mode)))
   ((eq major-mode 'js-ts-mode)
    (js-mode))
   ((eq major-mode 'typescript-mode)
    (when (and (fboundp 'treesit-language-available-p)
               (treesit-language-available-p 'typescript))
      (typescript-ts-mode)))
   ((eq major-mode 'typescript-ts-mode)
    (typescript-mode))
   (t (message "Not in a JavaScript or TypeScript buffer"))))

(defun javascript-install-tree-sitter-grammars ()
  "Install JavaScript/TypeScript tree-sitter grammars."
  (interactive)
  (if (and (fboundp 'treesit-available-p) (treesit-available-p))
      (dolist (lang '(javascript typescript tsx json))
        (if (and (fboundp 'treesit-language-available-p)
                 (treesit-language-available-p lang))
            (message "✓ %s grammar already installed" lang)
          (progn
            (message "Installing %s grammar..." lang)
            (condition-case err
                (progn
                  (treesit-install-language-grammar lang)
                  (message "✓ %s grammar installed successfully" lang))
              (error (message "✗ Failed to install %s grammar: %s" lang err))))))
    (message "Tree-sitter is not available in this Emacs build")))

;;; Mode Line Indicators
(defun javascript-mode-line-setup ()
  "Set up mode line for JavaScript development."
  (when (bound-and-true-p lsp-mode)
    (setq mode-name (concat mode-name " LSP")))
  (when (bound-and-true-p company-mode)
    (setq mode-name (concat mode-name " Co")))
  (when (bound-and-true-p flycheck-mode)
    (setq mode-name (concat mode-name " FC"))))

;; Apply to all JS/TS modes
(dolist (mode '(js-mode js2-mode js-ts-mode
                typescript-mode typescript-ts-mode
                tsx-ts-mode rjsx-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'javascript-mode-line-setup))

(provide 'init-javascript)
;;; init-javascript.el ends here
