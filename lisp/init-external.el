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
  '("application/x-compressed-tar" "application/zip" "application/json" "application/gzip" "application/x-shellscript")
  "List of file types that should still be opened in Emacs.
Emacs supports opening archive files via TRAMP,
and some users may have other files that need to be opened in Emacs."
  :group 'externalopen
  :type 'list)

(defcustom externalopen-desktop-blacklist
  '("emacs.desktop" "emacsclient.desktop")
  "List of programs that should not be opened externally."
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

(defun externalopen/find-program (file)
  "Check to see if FILE should be openned with an external program.
If so, then return the string representing the external program.
Nil is returned if the file should be opened within Emacs."
  (let ((file-type (externalopen/trim-newline (shell-command-to-string (concat "xdg-mime query filetype " file)))))
    (if (or (and (>= (length file-type) 4) (string= (substring file-type 0 4) "text"))
            (externalopen/find externalopen-keep-in-emacs (lambda (elem) (string= elem file-type))))
        nil ;; the file is a text file or on the whitelist
      (let ((desktop-file (externalopen/trim-newline (shell-command-to-string (concat "xdg-mime query default " file-type)))))
        (if (externalopen/find externalopen-desktop-blacklist (lambda (elem) (string= elem desktop-file)))
            ;; if the program/desktop file is on the blacklist, then return nil
            nil
          (let ((desktop-file-path (externalopen/find '("/usr/share/applications/" "/usr/local/share/applications/" "~/.local/share/applications/") (lambda (elem) (file-exists-p (concat elem desktop-file))))))
            (if desktop-file-path
                ;; if the desktop file exists, then need to parse it and get the name of the program to externally open the file with. use gnu-coreutils programs for this since lisp is slow
                (let ((external-program-line-string
                       (externalopen/trim-newline
                        (shell-command-to-string
                         (concat "grep '^Exec=' " desktop-file-path desktop-file " | head -1")
                         ))
                       ))
                  (if (> (length external-program-line-string) (length "Exec="))
                      (externalopen/extract-command external-program-line-string) ; return the external program name as a string
                    nil ; there was a desktop file, but no command was listed
                    )
                  )
              nil ; there is no desktop file that will point to an external program
              )
            )
          )
        )
      )
    ))

(defun externalopen/open-with-program (file program-command)
  "Open FILE with PROGRAM-COMMAND found based on xdg-mime type."
  (let ((shell-file-name "/bin/sh"))
    (message (concat "program-command: " program-command))
    (start-process-shell-command
     "openwith-process" nil
     (concat
      "exec nohup " program-command " " file " > /dev/null"))
    (kill-buffer nil)
    (error "Opened %s in external program: %s"
           (file-name-nondirectory file)
           program-command)
    ))

;; DELETE
(defun externalopen/file-handler (operation &rest args)
  "Open file with external program, if the xdg-mime is not text or on the whitelist."
  (when (and externalopen-mode (not (buffer-modified-p)) (zerop (buffer-size)))
    (let ((file (car args)))

      (let ((external-program (externalopen/find-program file)))
        (when external-program
          (externalopen/open-with-program file external-program)
          ))
      ))
  ;; if no external program was found/was a text file/was in the whitelist
  ;; basically if the find external program function returns null?
  (let ((inhibit-file-name-handlers
         (cons 'externalopen/find-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun externalopen/find-file-hook ()
  "The find file hook were 'buffer-file-name' is given as an arg."
  (when externalopen-mode
    (let ((file (buffer-file-name)))

      (let ((external-program (externalopen/find-program file)))
        (when external-program
          (externalopen/open-with-program file external-program)
          ))
      ))
  )

(define-minor-mode externalopen-mode
  "Open non text files with external applications found by xdg-open."
  :lighter ""
  :global t
  (if (and externalopen-mode (eq system-type 'gnu/linux))
      ;; (progn
      (add-hook 'find-file-hook 'externalopen/find-file-hook)
    ;;   (put 'externalopen/file-handler 'safe-magic t)
    ;;   (put 'externalopen/file-handler 'operations '(insert-file-contents))
    ;;   (add-to-list 'file-name-handler-alist '("" . externalopen/file-handler)))
    (remove-hook 'find-file-hook 'externalopen/find-file-hook)
    ;; (setq file-name-handler-alist
    ;;       (delete '("" . externalopen/file-handler) file-name-handler-alist))
    ))

;; (setq openwith-associations '(("\\.pdf\\'" "okular" (file))
;;                               ("\\.png\\'" "eog" (file))
;;                               ("\\.jpg\\'" "eog" (file))
;;                               ("\\.jpeg\\'" "eog" (file))
;;                               ("\\.mp3\\'" "vlc" (file))
;;                               ("\\.mp4\\'" "vlc" (file))
;;                               ))

(externalopen-mode t)


(provide 'init-external)
;;; init-external.el ends here
