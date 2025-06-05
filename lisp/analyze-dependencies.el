;;; analyze-dependencies.el --- Analyze dependencies between init files -*- lexical-binding: t -*-
;;; Commentary:
;; This file provides functions to analyze dependencies between init files
;; to help add proper require statements for compilation.
;;; Code:

(defvar init-dependency-cache nil
  "Cache of analyzed dependencies.")

(defun analyze-file-dependencies (file)
  "Analyze dependencies for a single init FILE.
Returns a list of symbols that this file depends on."
  (let ((dependencies '())
        (content ""))

    ;; Read file content
    (with-temp-buffer
      (insert-file-contents file)
      (setq content (buffer-string)))

    ;; Look for various dependency patterns
    (let ((patterns (list
                     ;; straight-use-package calls
                     "\\(straight-use-package\\s-+'\\([^)]+\\)\\)"
                     ;; after-load calls
                     "\\(after-load\\s-+'\\([^)]+\\)\\)"
                     ;; Function calls that might be from other init files
                     "\\(sanityinc/[a-zA-Z0-9-]+\\)"
                     ;; Common init functions
                     "\\(add-auto-mode\\|diminish\\|maybe-require-package\\)"
                     ;; require statements (to see existing deps)
                     "\\(require\\s-+'\\([^)]+\\)\\)")))

      (dolist (pattern patterns)
        (let ((pos 0))
          (while (string-match pattern content pos)
            (let ((match (match-string 2 content)))
              (when match
                (push (intern match) dependencies)))
            (setq pos (match-end 0))))))

    ;; Remove duplicates and return
    (delete-dups dependencies)))

(defun find-init-file-defining-symbol (symbol)
  "Find which init file likely defines SYMBOL."
  (let ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
        (found-in nil))

    (dolist (file (directory-files lisp-dir t "^init-.*\\.el$"))
      (when (and (not found-in)
                 (with-temp-buffer
                   (insert-file-contents file)
                   (goto-char (point-min))
                   (or (re-search-forward (format "(defun %s\\b" symbol) nil t)
                       (re-search-forward (format "(defvar %s\\b" symbol) nil t)
                       (re-search-forward (format "(defcustom %s\\b" symbol) nil t)
                       (re-search-forward (format "(defconst %s\\b" symbol) nil t))))
        (setq found-in (file-name-sans-extension
                        (file-name-nondirectory file)))))
    found-in))

(defun analyze-all-init-dependencies ()
  "Analyze dependencies for all init files."
  (interactive)
  (let ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
        (results '()))

    (message "Analyzing dependencies...")

    (dolist (file (directory-files lisp-dir t "^init-.*\\.el$"))
      (let* ((filename (file-name-sans-extension
                        (file-name-nondirectory file)))
             (deps (analyze-file-dependencies file))
             (missing-requires '()))

        ;; Check which dependencies need require statements
        (dolist (dep deps)
          (let ((provider (find-init-file-defining-symbol dep)))
            (when (and provider
                       (not (string= provider filename))
                       (not (string-match "^init-" (symbol-name dep))))
              (push provider missing-requires))))

        (push (list filename
                    (delete-dups missing-requires)
                    deps) results)))

    (setq init-dependency-cache results)
    (display-dependency-analysis results)))

(defun display-dependency-analysis (results)
  "Display the dependency analysis RESULTS."
  (with-output-to-temp-buffer "*Init Dependencies*"
    (princ "=== INIT FILE DEPENDENCY ANALYSIS ===\n\n")

    (dolist (item results)
      (let ((filename (car item))
            (missing-requires (cadr item))
            (all-deps (caddr item)))

        (princ (format "File: %s.el\n" filename))

        (if missing-requires
            (progn
              (princ "  Missing requires:\n")
              (dolist (req (delete-dups missing-requires))
                (princ (format "    (require '%s)\n" req))))
          (princ "  No missing requires found.\n"))

        (when all-deps
          (princ "  All dependencies found:\n")
          (dolist (dep (delete-dups all-deps))
            (princ (format "    %s\n" dep))))

        (princ "\n")))

    (princ "\nTo add missing requires to files:\n")
    (princ "M-x add-missing-requires-to-files\n")))

(defun scan-for-undefined-functions ()
  "Scan for function calls that might be undefined when compiling."
  (interactive)
  (let ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
        (undefined-functions '()))

    (dolist (file (directory-files lisp-dir t "^init-.*\\.el$"))
      (let ((filename (file-name-nondirectory file))
            (file-undefined '()))

        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))

          ;; Look for function calls
          (while (re-search-forward "\\([a-zA-Z0-9/-]+\\)\\s-*(" nil t)
            (let ((func-name (match-string 1)))
              (when (and (not (member func-name '("if" "when" "unless" "let" "let*"
                                                   "progn" "save-excursion" "with-temp-buffer"
                                                   "dolist" "while" "cond" "condition-case")))
                         (not (fboundp (intern func-name)))
                         (not (string-match "^[0-9]+$" func-name)))
                (push func-name file-undefined)))))

        (when file-undefined
          (push (cons filename (delete-dups file-undefined)) undefined-functions))))

    (with-output-to-temp-buffer "*Undefined Functions*"
      (princ "=== POTENTIALLY UNDEFINED FUNCTIONS ===\n\n")
      (if undefined-functions
          (dolist (item undefined-functions)
            (princ (format "File: %s\n" (car item)))
            (dolist (func (cdr item))
              (princ (format "  - %s\n" func)))
            (princ "\n"))
        (princ "No undefined functions found.\n")))))

(defun add-missing-requires-to-files ()
  "Add missing require statements to init files based on analysis."
  (interactive)
  (unless init-dependency-cache
    (analyze-all-init-dependencies))
  
  (let ((files-modified 0))
    (dolist (item init-dependency-cache)
      (let* ((filename (car item))
             (missing-requires (cadr item))
             (filepath (expand-file-name (concat filename ".el") 
                                         (expand-file-name "lisp" user-emacs-directory))))
        
        (when missing-requires
          (let ((content "")
                (actually-missing '()))
            
            ;; Read file content first
            (with-temp-buffer
              (insert-file-contents filepath)
              (setq content (buffer-string)))
            
            ;; Check which requires are actually missing
            (dolist (req missing-requires)
              (unless (string-match (format "(require '%s)" req) content)
                (push req actually-missing)))
            
            ;; Only modify file if there are actually missing requires
            (when actually-missing
              (with-temp-file filepath
                (insert content)
                (goto-char (point-min))
                
                ;; Find a good place to insert requires (after the commentary)
                (if (re-search-forward "^;;; Code:" nil t)
                    (progn
                      (forward-line 1)
                      (insert "\n;; Dependencies\n")
                      (dolist (req (reverse actually-missing))
                        (insert (format "(require '%s)\n" req)))
                      (insert "\n"))
                  ;; If no "Code:" section, insert after file header
                  (goto-char (point-min))
                  (forward-line 3)
                  (insert "\n;; Dependencies\n")
                  (dolist (req (reverse actually-missing))
                    (insert (format "(require '%s)\n" req)))
                  (insert "\n")))
              
              (setq files-modified (1+ files-modified))
              (message "Added %d requires to %s.el: %s" 
                       (length actually-missing)
                       filename 
                       (string-join (mapcar 'symbol-name actually-missing) ", ")))))))
    
    (if (> files-modified 0)
        (message "Modified %d files with missing requires" files-modified)
      (message "No files needed modification - all requires already present"))))

(defun check-circular-dependencies ()
  "Check for circular dependencies between init files."
  (interactive)
  (message "Checking for circular dependencies...")
  ;; This is a simplified check - a full implementation would need
  ;; a proper graph traversal algorithm
  (message "Circular dependency check not yet implemented"))

(provide 'analyze-dependencies)
;;; analyze-dependencies.el ends here
