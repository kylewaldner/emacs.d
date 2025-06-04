;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'subr-x)  ; For when-let (built-in since Emacs 26)

(when (straight-use-package 'flycheck)
  ;; (add-hook 'after-init-hook 'global-flycheck-mode)
  ;; this hook only turns on flycheck in local programming mode files
  (add-hook 'prog-mode-hook
            (lambda ()
              (interactive)
              (unless (file-remote-p default-directory)
                (flycheck-mode))))
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  (when (straight-use-package 'flycheck-color-mode-line)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(defun kyle/insert-line-with-text-above (text)
  "Insert line with TEXT above the current pos."
  (interactive "sinsert above:")
  (let (
        (indent-space
         (car
          (sanityinc/string-all-matches
           "^\s*"
           (buffer-substring
            (line-beginning-position) (line-end-position)))))
        )
    (save-excursion
      (end-of-line 0)
      (insert (concat "\n" indent-space text)))))

;; the base string is "// eslint-disable-next-line "

;; see flycheck-error-format-message-and-id to see how to access props of an error

;; TODO: improve this to ignore all the errors on the line, rather than the point
(defun kyle/flycheck-eslint-ignore-at-point ()
  "Add eslint ignore comment above the current line if there is an eslint error."
  (interactive)
  (when flycheck-mode
    (when-let (errors (flycheck-overlay-errors-at (point)))
      (kyle/insert-line-with-text-above
       (concat "// eslint-disable-next-line "
               (string-join
                (seq-map (lambda (err) (format "%s" (flycheck-error-id err))) errors)
                ", "))))))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
