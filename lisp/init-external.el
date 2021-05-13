;;; init-external.el --- opening external programs from emacs -*- lexical-binding: t -*-

;;; Commentary:
;;; Emacs pdf viewer is garbage.
;;; Code:

;; open all text/?+ files with emacs,
;; otherwise, use an external program

(defgroup externalopen nil
  "Open certain file types with external applications."
  :group 'files
  :group 'processes)

(defcustom externalopen-keep-in-emacs
  '("application/x-compressed-tar" "application/zip" "application/json")
  "List of file types that should still be opened in Emacs.
Emacs supports opening archive files via TRAMP,
and some users may have other files that need to be opened in Emacs."
  :group 'externalopen
  :type 'list)

(defcustom externalopen-ask-before-open nil
  "Prompt user before opening an external program."
  :group 'externalopen
  :type 'boolean)

(defun externalopen/find (list predicate-function)
  "Find the first element in the LIST where (PREDICATE-FUNCTION element) return t. Return nil if no such item is found."
  (if (not list)
      nil
    (if (funcall predicate-function (car list))
        (car list)
      (externalopen/find (cdr list) predicate-function)))
  )

;; TODO: update this func to only trim a newline if one is actually at the end
(defun externalopen/trim-newline (string)
  "Trims newlines from STRING."
  (let ((strlen (length string)))
    (if (and
         (> strlen 0)
         (string=
          (substring string -1)
          "\n"))
        (substring string 0 (- strlen 1)) ;then there is newline at the end that needs to be trimmed
      string ; else there is no newline that needs to be trimmed
      )
    )
  )

(defun externalopen/extract-command (exec-line)
  "Extracts the command string from an EXEC-LINE."
  (car (split-string (substring exec-line (length "Exec=")) " ")))

;; TODO: need way to see that we are in file archive

(defun externalopen/open-file-with (file)
  "Check to see if FILE should be openned with an external program."
  (interactive)
  (message file)
  (message (externalopen/trim-newline (shell-command-to-string (concat "xdg-mime query filetype " file))))
  (let ((file-type (externalopen/trim-newline (shell-command-to-string (concat "xdg-mime query filetype " file)))))
    (progn
      (message (concat "file-type: " file-type))
      (if (or (and (>= (length file-type) 4) (string= (substring file-type 0 4) "text"))
              (externalopen/find externalopen-keep-in-emacs (lambda (elem) (string= elem file-type))))
          ()
        (let ((desktop-file (externalopen/trim-newline (shell-command-to-string (concat "xdg-mime query default " file-type)))))
          (let ((desktop-file-path (externalopen/find '("/usr/share/applications/" "/usr/local/share/applications/" "~/.local/share/applications/") (lambda (elem) (file-exists-p (concat elem desktop-file))))))
            (if desktop-file-path
                                        ; if the desktop file exists, then need to parse it and get the name of the program to externally open the file with. use gnu-coreutils programs for this since lisp is slow
                (let ((external-program-line-string
                       (externalopen/trim-newline
                        (shell-command-to-string
                         (concat "grep '^Exec=' " desktop-file-path desktop-file " | head -1")
                         ))
                       ))
                  ;; (message (concat "done:: " ""))
                  (if (> (length external-program-line-string) (length "Exec="))
                                        ; then found the program to run it
                      (progn
                        (let ((program-command (externalopen/extract-command external-program-line-string))
                              (shell-file-name "/bin/sh"))
                          (progn
                            (message (concat "program-command: " program-command))
                            (start-process-shell-command
                             "openwith-process" nil
                             (concat
                              "exec nohup " program-command " " file " > /dev/null")))
                          )
                        (kill-buffer nil)
                        (error "Opened %s in external program"
                               (file-name-nondirectory file)))
                    (error "Could not find default program to open the file -- error 2"))
                  )

              (error "Could not find default program to open the file!") ; else, display an error message and dont open the file?
              )
            )

          )
        ))
    )
  )

(defun externalopen/find-file-hook ()
  "The find file hook were 'buffer-file-name' is given as an arg."
  (externalopen/open-file-with buffer-file-name))


;; (add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

(define-minor-mode externalopen-mode
  "Open non text files with external applications found by xdg-open."
  :lighter ""
  :global t
  (if externalopen-mode
      (progn
        (add-hook 'find-file-hook 'externalopen/find-file-hook)
        )
    (remove-hook 'find-file-hook 'externalopen/find-file-hook)))

;; (setq openwith-associations '(("\\.pdf\\'" "okular" (file))
;;                               ("\\.png\\'" "eog" (file))
;;                               ("\\.jpg\\'" "eog" (file))
;;                               ("\\.jpeg\\'" "eog" (file))
;;                               ("\\.mp3\\'" "vlc" (file))
;;                               ("\\.mp4\\'" "vlc" (file))
;;                               ))

;; (externalopen-mode t)


(provide 'init-external)
;;; init-external.el ends here
