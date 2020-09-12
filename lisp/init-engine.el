;;; init-engine.el --- engine mode customization -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; engine/keymap-prefix is C-x / by default

;; (engine/set-keymap-prefix (kbd "C-c s")) ;; to change the prefix

;; https://github.com/hrs/engine-mode

(when (maybe-require-package 'engine-mode)
  (engine-mode t)
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d"))

(provide 'init-engine)
;;; init-engine.el ends here
