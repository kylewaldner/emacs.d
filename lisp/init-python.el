;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; See the following note about how I set up python + virtualenv to
;; work seamlessly with Emacs:
;; https://gist.github.com/purcell/81f76c50a42eee710dcfc9a14bfc7240


(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(setq python-shell-interpreter "python3")

;;; pip3 install jedi autopep8 flake8 ipython importmagic yapf

(setq python-shell-interpreter-args "-i --simple-prompt")

(setq python-indent-offset 4)

(require-package 'pip-requirements)

(require-package 'jedi)

(when (maybe-require-package 'elpy)
  (elpy-enable)
  (after-load 'flycheck
    ;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)
    ;; (setq elpy-rpc-python-command "python3")
    ;; (setq flycheck-python-flake8-executable "python3")
    (when (maybe-require-package 'py-autopep8)
      ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
      ;; remove this hook to turn off python autofix - important in legacy repos
      ;; (add-hook 'after-save-hook (lambda ()
      ;;                              (if (and (eq major-mode 'python-mode) nil)
      ;;                                  (elpy-autopep8-fix-code))))
      )))


(setq elpy-rpc-timeout 2)

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-M-n") 'python-nav-up-list)))

(defun kyle/jedi:goto-definition-in-other-window ()
  "what that says"
  (interactive)
  (jedi:goto-definition 'other-window))

(require 'highlight-indentation)

(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)

;; py-autopep8 is other option
;; blacken is better since it runs inside emacs


;; (when (maybe-require-package 'blacken)
;;   (add-hook 'python-mode-hook 'blacken-mode))

;;; C-c C-c to bring up python buffer for buffer or region
;;; M-x pyvenv-workon to choose existing python virtualenv


(when (maybe-require-package 'anaconda-mode)
  (after-load 'python
    ;; Anaconda doesn't work on remote servers without some work, so
    ;; by default we enable it only when working locally.
    (add-hook 'python-mode-hook
              (lambda () (unless (file-remote-p default-directory)
                      (anaconda-mode 1))))
    (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode))
  (after-load 'anaconda-mode
    (define-key anaconda-mode-map (kbd "M-?") nil))
  (when (maybe-require-package 'company-anaconda)
    (after-load 'company
      (after-load 'python
        (push 'company-anaconda company-backends)))))


(provide 'init-python)
;;; init-python.el ends here
