;;; init-proto.el --- Support for protobuf -*- lexical-binding: t -*-
;;; Commentary:
;;; works with proto
;;; Code:

;; Ensure LSP unified is loaded for shared functionality
(require 'init-lsp-unified)

;; Install protobuf-mode
(straight-use-package
 '(protobuf-mode
   :type git
   :host github
   :repo "protocolbuffers/protobuf"
   :files ("editors/protobuf-mode.el")))

;; Basic mode setup
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; LSP server configuration for protobuf
(with-eval-after-load 'lsp-mode
  ;; Register protobuf language servers with alternatives
  ;; protobuf-language-server is the most actively maintained option
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "protobuf-language-server")
    :major-modes '(protobuf-mode)
    :server-id 'protobuf-language-server
    :priority 9))

  ;; Alternative: pbls (another protobuf language server)
  (when (executable-find "pbls")
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "pbls")
      :major-modes '(protobuf-mode)
      :server-id 'pbls
      :priority 8)))

  ;; Alternative: buf ls (if available in future buf releases)
  (when (executable-find "buf")
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("buf" "beta" "lsp"))
      :major-modes '(protobuf-mode)
      :server-id 'buf-lsp
      :priority 7))))

;; Protobuf-specific settings
(defun protobuf-setup ()
  "Setup protobuf mode with LSP integration."
  ;; Enable LSP mode
  (lsp-deferred)

  ;; Protobuf-specific indentation
  (setq-local tab-width 2
              indent-tabs-mode nil
              c-basic-offset 2)

  ;; Enable standard LSP keybindings
  (lsp-setup-standard-keybindings)

  ;; Protobuf-specific formatting
  (when (executable-find "buf")
    (setq-local lsp-enable-on-type-formatting t)))

;; Hook into protobuf-mode
(add-hook 'protobuf-mode-hook 'protobuf-setup)

;; Additional protobuf tools integration
(defun protobuf-format-buffer ()
  "Format the current protobuf buffer using buf format."
  (interactive)
  (if (and (executable-find "buf") (buffer-file-name))
      (let ((buf-command '("buf" "format" "--write")))
        (apply #'call-process (car buf-command) nil nil nil (cdr buf-command) (list (buffer-file-name)))
        (revert-buffer t t t))
    (message "buf not found or buffer not visiting a file")))

(defun protobuf-lint-buffer ()
  "Lint the current protobuf buffer using buf lint."
  (interactive)
  (if (and (executable-find "buf") (buffer-file-name))
      (let ((output (shell-command-to-string
                     (format "buf lint %s" (shell-quote-argument (buffer-file-name))))))
        (if (string-empty-p output)
            (message "No lint issues found")
          (with-output-to-temp-buffer "*buf lint*"
            (princ output))))
    (message "buf not found or buffer not visiting a file")))

;; Key bindings for protobuf-specific functions
(with-eval-after-load 'protobuf-mode
  (define-key protobuf-mode-map (kbd "C-c C-f") 'protobuf-format-buffer)
  (define-key protobuf-mode-map (kbd "C-c C-l") 'protobuf-lint-buffer))

;; Company completion for protobuf (when available)
(with-eval-after-load 'company
  (add-hook 'protobuf-mode-hook 'company-mode))

;; Flycheck integration for protobuf
(with-eval-after-load 'flycheck
  (add-hook 'protobuf-mode-hook 'flycheck-mode))

;; Display information about protobuf LSP setup
(defun protobuf-show-lsp-info ()
  "Display information about protobuf LSP setup."
  (interactive)
  (with-help-window "*Protobuf LSP Info*"
    (princ "Protobuf LSP Configuration\n")
    (princ "==========================\n\n")
    (princ "Available Language Servers (in priority order):\n")
    (princ "1. protobuf-language-server - Primary choice\n")
    (princ "   Install: go install github.com/lasorda/protobuf-language-server@master\n\n")
    (princ "2. pbls - Alternative option\n")
    (princ "   Install: cargo install --git https://github.com/rcorre/pbls\n\n")
    (princ "3. buf beta lsp - Future option (experimental)\n")
    (princ "   Install: brew install bufbuild/buf/buf\n\n")
    (princ "Additional Tools:\n")
    (princ "- buf format: C-c C-f\n")
    (princ "- buf lint: C-c C-l\n\n")
    (princ "Standard LSP Commands available via C-c l prefix\n")
    (princ "Use M-x lsp-show-keybindings for full LSP help\n")))

(provide 'init-proto)
;;; init-proto.el ends here
