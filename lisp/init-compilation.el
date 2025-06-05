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
         (total-files 0)
         (failed-files '()))

    (unless (file-directory-p lisp-dir)
      (error "Lisp directory not found: %s" lisp-dir))

    (message "Starting byte compilation of init files in: %s" lisp-dir)

    ;; Ensure load-path includes our directories
    (add-to-list 'load-path lisp-dir)
    (add-to-list 'load-path user-emacs-directory)

    ;; Compile all .el files in lisp directory
    (dolist (file (directory-files lisp-dir t "\\.el$"))
      (setq total-files (1+ total-files))
      (let ((filename (file-name-nondirectory file)))
        (message "Processing: %s..." filename)

        ;; Check if file has no-byte-compile directive
        (if (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (re-search-forward "^;;.*no-byte-compile:\\s-*t" nil t))
            (message "Skipping %s (no-byte-compile directive)" filename)

          ;; Try to compile the file
          (condition-case err
              (progn
                (when (byte-compile-file file)
                  (setq compiled-count (1+ compiled-count))
                  (message "✓ Compiled: %s" filename)))
            (error
             (push filename failed-files)
             (message "✗ Failed to compile %s: %s" filename (error-message-string err)))))))

    (message "\nByte compilation complete:")
    (message "  Successfully compiled: %d/%d files" compiled-count total-files)
    (when failed-files
      (message "  Failed files: %s" (string-join failed-files ", ")))

    ;; Return summary
    (list :compiled compiled-count :total total-files :failed failed-files)))

(defun clean-compiled-files ()
  "Remove all .elc files from the lisp directory."
  (interactive)
  (let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
         (deleted-count 0))

    (unless (file-directory-p lisp-dir)
      (error "Lisp directory not found: %s" lisp-dir))

    (message "Cleaning compiled files from: %s" lisp-dir)

    (dolist (file (directory-files lisp-dir t "\\.elc$"))
      (condition-case err
          (progn
            (delete-file file)
            (setq deleted-count (1+ deleted-count))
            (message "Deleted: %s" (file-name-nondirectory file)))
        (error
         (message "Failed to delete %s: %s"
                  (file-name-nondirectory file)
                  (error-message-string err)))))

    (message "Cleaned %d compiled files" deleted-count)
    deleted-count))

(defun recompile-init-files ()
  "Clean and recompile all init files."
  (interactive)
  (message "=== Recompiling all init files ===")
  (clean-compiled-files)
  (compile-init-files))

(defun compile-init-file-force (file)
  "Force compile a specific init file, even if up to date.
FILE should be the name without path or extension (e.g., `init-paredit')."
  (interactive "sInit file to compile (without .el): ")
  (let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
         (file-path (expand-file-name (concat file ".el") lisp-dir)))

    (unless (file-directory-p lisp-dir)
      (error "Lisp directory not found: %s" lisp-dir))

    (if (file-exists-p file-path)
        (progn
          (message "Force compiling: %s" file-path)

          ;; Delete existing .elc if it exists
          (let ((elc-path (concat file-path "c")))
            (when (file-exists-p elc-path)
              (delete-file elc-path)
              (message "Deleted existing: %s" (file-name-nondirectory elc-path))))

          ;; Add to load-path
          (add-to-list 'load-path lisp-dir)
          (add-to-list 'load-path user-emacs-directory)

          ;; Compile the file
          (condition-case err
              (progn
                (if (byte-compile-file file-path)
                    (message "✓ Successfully compiled %s.el" file)
                  (message "✗ Compilation returned nil for %s.el" file)))
            (error
             (message "✗ Failed to compile %s.el: %s" file (error-message-string err)))))
      (message "✗ File %s.el not found in lisp directory" file))))

(defun show-compilation-status ()
  "Show which init files are compiled and which are not."
  (interactive)
  (let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
         (compiled-files '())
         (uncompiled-files '())
         (no-compile-files '()))

    (unless (file-directory-p lisp-dir)
      (error "Lisp directory not found: %s" lisp-dir))

    (let ((el-files (directory-files lisp-dir nil "\\.el$")))
      (dolist (el-file el-files)
        (let* ((base-name (file-name-sans-extension el-file))
               (elc-file (concat base-name ".elc"))
               (elc-path (expand-file-name elc-file lisp-dir))
               (el-path (expand-file-name el-file lisp-dir)))

          ;; Check for no-byte-compile directive
          (if (with-temp-buffer
                (insert-file-contents el-path)
                (goto-char (point-min))
                (re-search-forward "^;;.*no-byte-compile:\\s-*t" nil t))
              (push el-file no-compile-files)
            (if (file-exists-p elc-path)
                (push el-file compiled-files)
              (push el-file uncompiled-files))))))

    (with-output-to-temp-buffer "*Compilation Status*"
      (princ "=== EMACS INIT FILES COMPILATION STATUS ===\n\n")
      (princ (format "Lisp directory: %s\n\n" lisp-dir))

      (princ (format "Compiled files (%d):\n" (length compiled-files)))
      (if compiled-files
          (dolist (file (sort compiled-files 'string<))
            (princ (format "  ✓ %s\n" file)))
        (princ "  (none)\n"))

      (princ (format "\nUncompiled files (%d):\n" (length uncompiled-files)))
      (if uncompiled-files
          (dolist (file (sort uncompiled-files 'string<))
            (princ (format "  ✗ %s\n" file)))
        (princ "  (none)\n"))

      (when no-compile-files
        (princ (format "\nFiles with no-byte-compile directive (%d):\n" (length no-compile-files)))
        (dolist (file (sort no-compile-files 'string<))
          (princ (format "  ○ %s\n" file))))

      (princ (format "\nTotal: %d files (%d compiled, %d uncompiled"
                      (+ (length compiled-files) (length uncompiled-files) (length no-compile-files))
                      (length compiled-files)
                      (length uncompiled-files)))
      (when no-compile-files
        (princ (format ", %d no-compile" (length no-compile-files))))
      (princ ")\n")

      (princ "\nTo compile all files: M-x compile-init-files\n")
      (princ "To recompile all: M-x recompile-init-files\n")
      (princ "To clean compiled files: M-x clean-compiled-files\n"))))

(defun show-compilation-status-batch ()
  "Show compilation status in batch mode (prints to stdout)."
  (interactive)
  (let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
         (compiled-files '())
         (uncompiled-files '())
         (no-compile-files '()))

    (unless (file-directory-p lisp-dir)
      (error "Lisp directory not found: %s" lisp-dir))

    (let ((el-files (directory-files lisp-dir nil "\\.el$")))
      (dolist (el-file el-files)
        (let* ((base-name (file-name-sans-extension el-file))
               (elc-file (concat base-name ".elc"))
               (elc-path (expand-file-name elc-file lisp-dir))
               (el-path (expand-file-name el-file lisp-dir)))

          ;; Check for no-byte-compile directive
          (if (with-temp-buffer
                (insert-file-contents el-path)
                (goto-char (point-min))
                (re-search-forward "^;;.*no-byte-compile:\\s-*t" nil t))
              (push el-file no-compile-files)
            (if (file-exists-p elc-path)
                (push el-file compiled-files)
              (push el-file uncompiled-files))))))

    (message "=== EMACS INIT FILES COMPILATION STATUS ===")
    (message "")
    (message "Lisp directory: %s" lisp-dir)
    (message "")

    (message "Compiled files (%d):" (length compiled-files))
    (if compiled-files
        (dolist (file (sort compiled-files 'string<))
          (message "  ✓ %s" file))
      (message "  (none)"))

    (message "")
    (message "Uncompiled files (%d):" (length uncompiled-files))
    (if uncompiled-files
        (dolist (file (sort uncompiled-files 'string<))
          (message "  ✗ %s" file))
      (message "  (none)"))

    (when no-compile-files
      (message "")
      (message "Files with no-byte-compile directive (%d):" (length no-compile-files))
      (dolist (file (sort no-compile-files 'string<))
        (message "  ○ %s" file)))

    (message "")
    (message "Total: %d files (%d compiled, %d uncompiled%s)"
             (+ (length compiled-files) (length uncompiled-files) (length no-compile-files))
             (length compiled-files)
             (length uncompiled-files)
             (if no-compile-files (format ", %d no-compile" (length no-compile-files)) ""))

    (message "")
    (message "To compile all files: M-x compile-init-files")
    (message "To recompile all: M-x recompile-init-files")
    (message "To clean compiled files: M-x clean-compiled-files")))

;; Auto-compile on save (optional - uncomment if desired)
;; (defun auto-compile-init-file ()
;;   "Automatically compile init file when saved."
;;   (when (and buffer-file-name
;;              (string-match "/lisp/init-.*\\.el$" buffer-file-name))
;;     (byte-compile-file buffer-file-name)))
;;
;; (add-hook 'after-save-hook 'auto-compile-init-file)

(defun show-startup-timing ()
  "Show startup timing information."
  (interactive)
  (with-output-to-temp-buffer "*Startup Timing*"
    (princ "=== EMACS STARTUP TIMING ===\n\n")

    ;; Basic startup time
    (if (and (boundp 'before-init-time) (boundp 'after-init-time)
             before-init-time after-init-time)
        (princ (format "Startup time: %.2f seconds\n"
                       (float-time (time-subtract after-init-time before-init-time))))
      (princ "Startup time: Not available (timing variables not set)\n"))

    ;; Emacs version and configuration info
    (princ (format "Emacs version: %s\n" emacs-version))
    (princ (format "Configuration directory: %s\n\n" user-emacs-directory))

    ;; Show init file load order and times
    (when (boundp 'sanityinc/require-times)
      (princ (format "Total modules loaded: %d\n" (length sanityinc/require-times)))
      (princ "\nSlowest loading modules:\n")
      (let ((sorted-times (sort (copy-sequence sanityinc/require-times)
                                (lambda (a b) (> (caddr a) (caddr b))))))
        (dolist (item (cl-subseq sorted-times 0 (min 15 (length sorted-times))))
          (princ (format "  %6.0fms  %s\n" (caddr item) (car item)))))

      ;; Show timing summary
      (let ((total-time (apply '+ (mapcar 'caddr sanityinc/require-times)))
            (gc-time (if (boundp 'gcs-done)
                         (* (float gcs-done) (if (boundp 'gc-elapsed) gc-elapsed 0))
                       0)))
        (princ (format "\nTotal require time: %.0fms\n" total-time))
        (when (> gc-time 0)
          (princ (format "Garbage collection time: %.0fms (%d collections)\n"
                         (* gc-time 1000) gcs-done))))

      ;; Offer to show detailed view
      (princ "\nFor detailed module loading times, run: M-x sanityinc/require-times\n"))

    (unless (boundp 'sanityinc/require-times)
      (princ "Detailed timing data not available.\n")
      (princ "Make sure init-benchmarking.el is loaded early in your configuration.\n"))

    ;; Show compilation status
    (princ "\n=== COMPILATION STATUS ===\n")
    (let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
           (el-files (when (file-directory-p lisp-dir)
                       (directory-files lisp-dir nil "\\.el$")))
           (compiled-count 0))
      (when el-files
        (dolist (el-file el-files)
          (let* ((base-name (file-name-sans-extension el-file))
                 (elc-file (concat base-name ".elc"))
                 (elc-path (expand-file-name elc-file lisp-dir)))
            (when (file-exists-p elc-path)
              (setq compiled-count (1+ compiled-count)))))
        (princ (format "Compiled files: %d/%d (%.1f%%)\n"
                       compiled-count (length el-files)
                       (* 100.0 (/ (float compiled-count) (length el-files))))))
      (unless el-files
        (princ "No .el files found in lisp directory.\n")))))

(defun show-current-startup-time ()
  "Show the startup time of the current Emacs session."
  (interactive)
  (if (and (boundp 'before-init-time) (boundp 'after-init-time)
           before-init-time after-init-time)
      (message "Current session startup time: %.2f seconds"
               (float-time (time-subtract after-init-time before-init-time)))
    (message "Startup timing not available for current session")))

(provide 'init-compilation)
;;; init-compilation.el ends here
