;;; init-claude.el --- Claude Code support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Install inheritenv dependency
(straight-use-package
 '(inheritenv :type git :host github :repo "purcell/inheritenv"))

;; Install eat terminal backend
(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

;; Install vterm terminal backend
(straight-use-package 'vterm)

;; Install claude-code.el, using :depth 1 to reduce download size
(when (straight-use-package
       '(claude-code :type git
                     :host github
                     :repo "stevemolitor/claude-code.el"
                     :branch "main"
                     :depth 1
                     :files ("*.el" (:exclude "images/*"))))

  ;; Set up keybindings
  (with-eval-after-load 'claude-code
    ;; Bind C-c c to the claude-code command map
    (global-set-key (kbd "C-c c") 'claude-code-command-map)

    ;; Optional: Define a repeat map for cycling modes
    (when (fboundp 'claude-code-cycle-mode)
      (define-key global-map (kbd "C-c M") 'claude-code-cycle-mode)))

  ;; Optional IDE integration with Monet
  (when (fboundp 'monet-start-server-function)
    (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
    (when (fboundp 'monet-mode)
      (monet-mode 1)))

  ;; Enable claude-code-mode
  (when (fboundp 'claude-code-mode)
    (claude-code-mode)))



(provide 'init-claude)
;;; init-claude.el ends here
