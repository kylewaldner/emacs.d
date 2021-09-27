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

(setq python-indent-offset 4)

(require-package 'pip-requirements)

(require-package 'jedi)

(when (maybe-require-package 'elpy)
  (elpy-enable)
  (after-load 'flycheck
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)
    (setq elpy-rpc-python-command "python3")
    (setq flycheck-python-flake8-executable "python3")
    (when (maybe-require-package 'py-autopep8)
      (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
      ;; remove this hook to turn off python autofix - important in legacy repos
      (add-hook 'after-save-hook (lambda ()
                                   (if (eq major-mode 'python-mode)
                                       (elpy-autopep8-fix-code))))
      )))


(setq elpy-rpc-timeout 10)

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
