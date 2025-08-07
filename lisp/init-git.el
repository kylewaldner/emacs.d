;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(straight-use-package 'git-blamed)
;; (straight-use-package 'gitignore-mode)
;; (straight-use-package 'gitconfig-mode)
;; instead use new bundled package
(straight-use-package 'git-modes)
(when (straight-use-package 'git-timemachine)
  (global-set-key (kbd "C-x v t") 'git-timemachine-toggle))



(when (straight-use-package 'magit)
  (setq-default magit-diff-refine-hunk t)

  ;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
  ;; quickly open magit on any one of your projects.
  (global-set-key [(meta f12)] 'magit-status)
  (global-set-key (kbd "C-x j") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch)

  (defun sanityinc/magit-or-vc-log-file (&optional prompt)
    (interactive "P")
    (if (and (buffer-file-name)
             (eq 'Git (vc-backend (buffer-file-name))))
        (if prompt
            (magit-log-buffer-file-popup)
          (magit-log-buffer-file t))
      (vc-print-log)))

  (straight-use-package '(magit-tagger :type git :host github :repo "kylewaldner/magit-tagger"))

  (after-load 'vc
    (define-key vc-prefix-map (kbd "l") 'sanityinc/magit-or-vc-log-file)))


;; Custom command to undo last commit and keep changes
(defun kyle/magit-undo-last-commit ()
  "Undo the last commit but keep the changes staged.
This runs 'git reset --soft HEAD~1'."
  (interactive)
  (if (magit-anything-staged-p)
      (when (yes-or-no-p "You have staged changes. Undo last commit anyway? ")
        (magit-run-git "reset" "--soft" "HEAD~1"))
    (magit-run-git "reset" "--soft" "HEAD~1"))
  (magit-refresh))

(after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)
  ;; Bind the undo last commit command to 'U' key
  (define-key magit-status-mode-map (kbd "M-u") 'kyle/magit-undo-last-commit)

  ;; Add the command to Magit's transient system so it appears in help menu
  (transient-append-suffix 'magit-dispatch "X"
    '("M-u" "Undo last commit (soft)" kyle/magit-undo-last-commit)))

(straight-use-package 'magit-todos)

(straight-use-package 'fullframe)
;; (after-load 'magit
;;   (fullframe magit-status magit-mode-quit-window))
;; dont force magit status to be fullscreen

(after-load 'magit
  (add-hook 'git-commit-mode-hook 'goto-address-mode))



(when *is-a-mac*
  (after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))



;; Convenient binding for vc-git-grep
(after-load 'vc
  (define-key vc-prefix-map (kbd "f") 'vc-git-grep))



;;; git-svn support

;; (when (straight-use-package 'magit-svn)
;;   (straight-use-package 'magit-svn)
;;   (autoload 'magit-svn-enabled "magit-svn")
;;   (defun sanityinc/maybe-enable-magit-svn-mode ()
;;     (when (magit-svn-enabled)
;;       (magit-svn-mode)))
;;   (add-hook 'magit-status-mode-hook #'sanityinc/maybe-enable-magit-svn-mode))

(after-load 'compile
  (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                      '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
    (add-to-list 'compilation-error-regexp-alist-alist defn)
    (add-to-list 'compilation-error-regexp-alist (car defn))))

(defvar git-svn--available-commands nil "Cached list of git svn subcommands")
(defun git-svn--available-commands ()
  (or git-svn--available-commands
      (setq git-svn--available-commands
            (sanityinc/string-all-matches
             "^  \\([a-z\\-]+\\) +"
             (shell-command-to-string "git svn help") 1))))

(autoload 'vc-git-root "vc-git")

(defun git-svn (dir command)
  "Run a git svn subcommand in DIR."
  (interactive (list (read-directory-name "Directory: ")
                     (completing-read "git-svn command: " (git-svn--available-commands) nil t nil nil (git-svn--available-commands))))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
    (compile (concat "git svn " command))))


(provide 'init-git)
;;; init-git.el ends here
