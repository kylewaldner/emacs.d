;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'projectile)
(straight-use-package 'ibuffer-projectile)

;; Load projectile first
(require 'projectile)

;; Configure projectile settings
(setq-default projectile-mode-line-prefix " Proj")
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'alien)
(setq projectile-git-command "git ls-files -zco --exclude-standard")
(setq projectile-git-ignored-command "git ls-files -zcoi --exclude-standard")
(setq projectile-use-git-grep t)

;; Define the reload function
(defun kyle/projectile-reload-current-project ()
  "Reload the current project and prompt for action."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (when project-root
      (projectile-invalidate-cache nil)
      (projectile-switch-project-by-name project-root))))

(defun kyle/projectile-fresh-start ()
  "Treat the current project as freshly cloned.
Removes project from known projects, clears all caches,
closes project buffers, and then re-adds and switches to it."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (when project-root
      (message "Giving %s a fresh start..." project-root)
      ;; Kill project buffers
      (projectile-kill-buffers)
      ;; Remove from known projects
      (projectile-remove-known-project project-root)
      ;; Clear all caches
      (projectile-invalidate-cache t)
      ;; Re-add the project
      (projectile-add-known-project project-root)
      ;; Switch to it
      (projectile-switch-project-by-name project-root)
      (message "Project %s has been reset!" project-root))))

;; Bind it to a key
(with-eval-after-load 'projectile
  (define-key projectile-command-map (kbd "F") 'kyle/projectile-fresh-start))

;; Now set up the keybindings and enable the mode
(with-eval-after-load 'projectile
  ;; Set the prefix key
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  ;; Add the reload function binding
  (define-key projectile-command-map (kbd "R") 'kyle/projectile-reload-current-project))

;; Enable projectile mode after configuration
(projectile-mode 1)

;; Compilation key
(global-set-key (kbd "<f5>") 'projectile-compile-project)

(provide 'init-projectile)
;;; init-projectile.el ends here
