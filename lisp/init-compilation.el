;;; init-compilation.el --- Byte compilation utilities -*- lexical-binding: t -*-
;;; Commentary:
;; Functions to byte-compile configuration files for better performance
;;; Code:

(defun compile-init-files ()
  "Byte-compile all init files in the lisp directory.
Respects the no-byte-compile directive in init.el."
  (interactive)
  (let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
         (compiled-count 0)
         (total-files 0))
    (message "Starting byte compilation of init files...")

    ;; Compile all .el files in lisp directory
    (dolist (file (directory-files lisp-dir t "\\.el$"))
      (setq total-files (1+ total-files))
      (condition-case err
          (when (byte-compile-file file)
            (setq compiled-count (1+ compiled-count))
            (message "Compiled: %s" (file-name-nondirectory file)))
        (error
         (message "Failed to compile %s: %s"
                  (file-name-nondirectory file)
                  (error-message-string err)))))

    (message "Byte compilation complete: %d/%d files compiled successfully"
             compiled-count total-files)))

(defun clean-compiled-files ()
  "Remove all .elc files from the lisp directory."
  (interactive)
  (let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
         (deleted-count 0))
    (dolist (file (directory-files lisp-dir t "\\.elc$"))
      (delete-file file)
      (setq deleted-count (1+ deleted-count))
      (message "Deleted: %s" (file-name-nondirectory file)))
    (message "Cleaned %d compiled files" deleted-count)))

(defun recompile-init-files ()
  "Clean and recompile all init files."
  (interactive)
  (clean-compiled-files)
  (compile-init-files))

(defun compile-init-file-force (file)
  "Force compile a specific init file, even if up to date.
FILE should be the name without path or extension (e.g., 'init-paredit')."
  (interactive "sInit file to compile (without .el): ")
  (let* ((file-path (expand-file-name (concat file ".el")
                                      (expand-file-name "lisp" user-emacs-directory))))
    (if (file-exists-p file-path)
        (progn
          ;; Delete existing .elc if it exists
          (let ((elc-path (concat file-path "c")))
            (when (file-exists-p elc-path)
              (delete-file elc-path)))
          ;; Compile the file
          (if (byte-compile-file file-path)
              (message "Successfully compiled %s.el" file)
            (message "Failed to compile %s.el" file)))
      (message "File %s.el not found in lisp directory" file))))

(defun show-compilation-status ()
  "Show which init files are compiled and which are not."
  (interactive)
  (let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
         (el-files (directory-files lisp-dir nil "\\.el$"))
         (compiled-files '())
         (uncompiled-files '()))

    (dolist (el-file el-files)
      (let* ((base-name (file-name-sans-extension el-file))
             (elc-file (concat base-name ".elc"))
             (elc-path (expand-file-name elc-file lisp-dir)))
        (if (file-exists-p elc-path)
            (push el-file compiled-files)
          (push el-file uncompiled-files))))

    (with-output-to-temp-buffer "*Compilation Status*"
      (princ "=== EMACS INIT FILES COMPILATION STATUS ===\n\n")
      (princ (format "Compiled files (%d):\n" (length compiled-files)))
      (dolist (file (sort compiled-files 'string<))
        (princ (format "  ✓ %s\n" file)))
      (princ (format "\nUncompiled files (%d):\n" (length uncompiled-files)))
      (dolist (file (sort uncompiled-files 'string<))
        (princ (format "  ✗ %s\n" file)))
      (princ (format "\nTotal: %d files (%d compiled, %d uncompiled)\n"
                      (+ (length compiled-files) (length uncompiled-files))
                      (length compiled-files)
                      (length uncompiled-files))))))

;; Auto-compile on save (optional - uncomment if desired)
;; (defun auto-compile-init-file ()
;;   "Automatically compile init file when saved."
;;   (when (and buffer-file-name
;;              (string-match "/lisp/init-.*\\.el$" buffer-file-name))
;;     (byte-compile-file buffer-file-name)))
;;
;; (add-hook 'after-save-hook 'auto-compile-init-file)

(provide 'init-compilation)
;;; init-compilation.el ends here
