;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(defun search-refresh-key (this-mode-map)
  "Set refresh key for this type of search mode.  THIS-MODE-MAP is the mode map to set."
  (dolist (key (list (kbd "C-c C-r") (kbd "r")))
    (define-key this-mode-map key 'revert-buffer)))

(straight-use-package 'wgrep)
(after-load 'grep
  (dolist (key (list (kbd "C-c C-q") (kbd "w")))
    (define-key grep-mode-map key 'wgrep-change-to-wgrep-mode))
  (search-refresh-key grep-mode-map))

(when (and (executable-find "ag")
           (straight-use-package 'ag))
  (straight-use-package 'wgrep-ag)
  (setq ag-arguments (list "--stats" "--smart-case" "-W" "100"))
  ;; C-C C-p to change to wgrep mode
  (setq-default ag-highlight-search t)
  ;; (search-refresh-key ag-mode-map) ;; ag-mode-map does not exist
  ;; use g to refresh buffer
  (global-set-key (kbd "C-c i") 'projectile-ag)) ;; global search in project

(when (and (executable-find "rg")
           (straight-use-package 'rg))
  (straight-use-package 'deadgrep)
  (global-set-key (kbd "M-?") 'rg-project))

;; grep-find-ignored-directories needs to be modified for rgrep to e useful

(provide 'init-grep)
;;; init-grep.el ends here
