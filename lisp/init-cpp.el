;;; init-cpp.el --- Support for C and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; http://blog.lujun9972.win/emacs-document/blog/2018/03/22/emacs-as-a-c++-ide/index.html


;;  If you are using Cmake to build your project, it is really easy to generate compilation database, since CMake has support for it. You just provide CMake with correct flag (cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ...) and that is it! Later you can just call cmake . and it will update compilation database if needed. This is what I used for my project, since it uses CMake.

(when (require 'irony)
  ;; If irony server was never installed, install it.
  (unless (irony--find-server-executable) (call-interactively #'irony-install-server))
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)

  ;; Use compilation database first, clang_complete as fallback.
  (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
                                                  irony-cdb-clang-complete))

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(when (maybe-require-package 'company-irony)
  (eval-after-load 'company '(add-to-list 'company-backends 'company-irony)))

(when (maybe-require-package 'flycheck-irony)
  (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(when (maybe-require-package 'irony-eldoc)
  (add-hook 'irony-mode-hook #'irony-eldoc))


(defun kyle/c-project-setup ()
  "Setup a the c project by generating compile commands with bear."
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (compile "bear make"))
  (call-interactively 'irony-cdb-json-add-compile-commands-path)
  )


(provide 'init-cpp)
;;; init-cpp.el ends here
