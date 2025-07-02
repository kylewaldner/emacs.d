;;; init-github.el --- Github integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-git)

(straight-use-package 'yagist)
(when (straight-use-package 'bug-reference-github)
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode))

(straight-use-package 'github-clone)
;; Suppress cl deprecation warning for forge
(with-suppressed-warnings ((obsolete cl-lib))
  (straight-use-package 'forge))
(straight-use-package 'github-review)

(defun github-link-at-point ()
  "Create a GitHub link for the current cursor position or selected region and copy it to clipboard.
   If a region is selected, the link will include the line range."
  (interactive)
  (let* ((use-region (use-region-p))
         (start-line (if use-region
                         (line-number-at-pos (region-beginning))
                       (line-number-at-pos)))
         (end-line (if use-region
                       (line-number-at-pos (region-end))
                     nil))
         ;; Adjust end-line if region ends at beginning of a line
         (end-line (if (and use-region
                            (= (region-end) (line-beginning-position (- (region-end) (point-at-bol) -1))))
                       (1- end-line)
                     end-line))
         (file-path (buffer-file-name))
         (git-root (locate-dominating-file default-directory ".git"))
         (relative-path (if (and file-path git-root)
                            (file-relative-name file-path git-root)
                          (error "Not in a git repository")))
         (default-directory git-root)
         (remote-url (string-trim (shell-command-to-string "git config --get remote.origin.url")))
         (github-info (if (string-match "\\(?:https://github.com/\\|git@github.com:\\)\\([^/]+\\)/\\([^/.]+\\)" remote-url)
                          (cons (match-string 1 remote-url) (match-string 2 remote-url))
                        (error "Not a GitHub repository")))
         (owner (car github-info))
         (repo (cdr github-info))
         (branch (string-trim (shell-command-to-string "git symbolic-ref --short HEAD 2>/dev/null || git rev-parse HEAD")))
         (line-ref (if (and use-region (not (= start-line end-line)))
                       (format "#L%d-L%d" start-line end-line)
                     (format "#L%d" start-line)))
         (github-url (format "https://github.com/%s/%s/blob/%s/%s%s"
                             owner repo branch relative-path line-ref)))
    (kill-new github-url)
    (message "Copied to clipboard: %s" github-url)))

(provide 'init-github)
;;; init-github.el ends here
